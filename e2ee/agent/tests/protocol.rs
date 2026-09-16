// Integration tests for the leman-agent JSON protocol.
//
// Tests drive the real binary over stdio (line-delimited JSON), as
// Leman.el does.  The round-trip tests pair the agent (Bob) with an
// in-process matrix-sdk-crypto machine (Alice), acting as a virtual
// homeserver shuttling outgoing requests between the two.

use std::collections::BTreeMap;
use std::io::{BufRead, BufReader, Write};
use std::process::{Child, Command, Stdio};

use matrix_sdk_crypto::{
    types::events::room::encrypted::EncryptedEvent,
    types::requests::{AnyOutgoingRequest, OutgoingVerificationRequest},
    DecryptionSettings, EncryptionSyncChanges, OlmMachine, TrustRequirement,
};
use ruma::api::client::keys::upload_keys::v3::Response as UploadKeysResponse;
use ruma::events::AnyToDeviceEvent;
use serde_json::{json, Value};
use tempfile::TempDir;

struct TestAgent {
    child: Child,
    stdout: BufReader<std::process::ChildStdout>,
    next_id: u64,
}

impl TestAgent {
    fn spawn() -> Self {
        let mut child = Command::new(env!("CARGO_BIN_EXE_leman-agent"))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .expect("failed to spawn agent");
        let stdout = child.stdout.take().unwrap();
        Self {
            child,
            stdout: BufReader::new(stdout),
            next_id: 1,
        }
    }

    fn send(&mut self, value: &Value) {
        let mut stdin = self.child.stdin.take().unwrap();
        writeln!(stdin, "{value}").unwrap();
        self.child.stdin = Some(stdin);
    }

    fn request(&mut self, cmd: &str, params: Value) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        let mut message = json!({"id": id, "cmd": cmd});
        if !params.is_null() {
            message["params"] = params;
        }
        self.send(&message);
        self.recv_response(id)
    }

    fn recv_response(&mut self, id: u64) -> Value {
        let mut line = String::new();
        self.stdout.read_line(&mut line).expect("read response");
        let response: Value = serde_json::from_str(&line).expect("response is valid JSON");
        assert_eq!(response["id"], json!(id), "response id mismatch");
        response
    }
}

impl Drop for TestAgent {
    fn drop(&mut self) {
        // Ask the agent to exit cleanly first: a SIGKILLed process
        // never writes its coverage profile.
        if let Some(mut stdin) = self.child.stdin.take() {
            let _ = writeln!(stdin, r#"{{"id":999999,"cmd":"quit"}}"#);
            drop(stdin);
            for _ in 0..100 {
                if matches!(self.child.try_wait(), Ok(Some(_))) {
                    break;
                }
                std::thread::sleep(std::time::Duration::from_millis(20));
            }
        }
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

fn raw_from_value<T>(value: &Value) -> ruma::serde::Raw<T>
where
    T: serde::de::DeserializeOwned,
{
    let raw_value = serde_json::value::RawValue::from_string(value.to_string()).unwrap();
    ruma::serde::Raw::from_json(raw_value)
}

fn initialize_params(store: &TempDir) -> Value {
    json!({
        "user_id": "@bob:example.org",
        "device_id": "BOBDEVICE",
        "store_path": store.path().to_str().unwrap(),
    })
}

/// Run the full key exchange between Alice (in-process) and Bob (the
/// agent), then have Alice encrypt one event.  Returns the encrypted
/// event, ready for `decrypt_room_event`.
async fn exchange_and_encrypt(agent: &mut TestAgent, alice: &OlmMachine) -> Value {
    use matrix_sdk_crypto::{types::requests::AnyOutgoingRequest, EncryptionSettings};
    use ruma::{
        api::client::keys::{
            claim_keys::v3::Response as ClaimKeysResponse,
            get_keys::v3::Response as GetKeysResponse,
            upload_keys::v3::Response as UploadKeysResponse,
        },
        device_id,
        encryption::{DeviceKeys, OneTimeKey},
        events::AnyToDeviceEvent,
        serde::Raw,
        user_id,
    };

    // Bob uploads his keys; the virtual homeserver stores the upload
    // body and answers with an empty response.
    let bob_upload = agent.request("outgoing_requests", json!({}));
    let request = &bob_upload["ok"]["requests"][0];
    assert!(
        request["path"].as_str().unwrap().contains("/keys/upload"),
        "expected keys upload request: {request}"
    );
    // The body is a JSON string, passed through the client verbatim.
    assert!(
        request["body"].is_string(),
        "request body must be a JSON string: {request}"
    );
    let body: Value =
        serde_json::from_str(request["body"].as_str().unwrap()).expect("valid body JSON");
    let bob_device_keys = body["device_keys"].clone();
    let bob_one_time_keys = body["one_time_keys"].clone();
    agent.request(
        "mark_request_as_sent",
        json!({"request_id": request["id"], "response": {"one_time_key_counts": {}}}),
    );

    // Alice uploads her keys, tracks Bob, and queries his device keys.
    for request in alice.outgoing_requests().await.unwrap() {
        if matches!(request.request(), AnyOutgoingRequest::KeysUpload(_)) {
            alice
                .mark_request_as_sent(
                    request.request_id(),
                    &UploadKeysResponse::new(BTreeMap::new()),
                )
                .await
                .unwrap();
        }
    }
    alice
        .update_tracked_users(vec![user_id!("@bob:example.org")])
        .await
        .unwrap();
    for request in alice.outgoing_requests().await.unwrap() {
        if let AnyOutgoingRequest::KeysQuery(_) = request.request() {
            let mut response = GetKeysResponse::new();
            response.device_keys.insert(
                user_id!("@bob:example.org").to_owned(),
                {
                    let mut devices = BTreeMap::new();
                    devices.insert(
                        device_id!("BOBDEVICE").to_owned(),
                        serde_json::from_value::<Raw<DeviceKeys>>(bob_device_keys.clone()).unwrap(),
                    );
                    devices
                },
            );
            alice
                .mark_request_as_sent(request.request_id(), &response)
                .await
                .unwrap();
        }
    }

    // Alice claims one of Bob's one-time keys to establish an Olm session.
    let (claim_id, _claim_request) = alice
        .get_missing_sessions(vec![user_id!("@bob:example.org")].into_iter())
        .await
        .unwrap()
        .expect("should want to claim keys");
    let mut claim_response = ClaimKeysResponse::new(BTreeMap::new());
    claim_response.one_time_keys.insert(
        user_id!("@bob:example.org").to_owned(),
        {
            let mut devices = BTreeMap::new();
            let key_id = bob_one_time_keys.as_object().unwrap().keys().next().unwrap().clone();
            let key_id = serde_json::from_value::<ruma::OwnedOneTimeKeyId>(json!(key_id)).unwrap();
            let key_value = bob_one_time_keys.as_object().unwrap().values().next().unwrap().clone();
            devices.insert(
                device_id!("BOBDEVICE").to_owned(),
                {
                    let mut keys = BTreeMap::new();
                    keys.insert(key_id, serde_json::from_value::<Raw<OneTimeKey>>(key_value).unwrap());
                    keys
                },
            );
            devices
        },
    );
    alice
        .mark_request_as_sent(&claim_id, &claim_response)
        .await
        .unwrap();

    // Alice establishes a session with Bob and shares the room key.
    let room_id = ruma::room_id!("!room:example.org");
    let share_requests = alice
        .share_room_key(
            room_id,
            vec![user_id!("@bob:example.org")].into_iter(),
            EncryptionSettings::default(),
        )
        .await
        .unwrap();
    let mut room_key_events = Vec::new();
    for send in share_requests {
        for devices in send.messages.values() {
            for content in devices.values() {
                room_key_events.push(raw_from_value::<AnyToDeviceEvent>(&json!({
                    "sender": "@alice:example.org",
                    "type": serde_json::to_value(&send.event_type).unwrap(),
                    "content": serde_json::to_value(content).unwrap(),
                })));
            }
        }
    }
    assert!(!room_key_events.is_empty());

    // Bob receives the room key through the agent protocol.
    let decrypted_to_device = agent.request(
        "receive_sync_changes",
        json!({
            "to_device_events": room_key_events.iter().map(|e| serde_json::to_value(e).unwrap()).collect::<Vec<_>>(),
            "changed_devices": {"changed": [], "left": []},
            "one_time_keys_count": {"signed_curve25519": 100},
            "unused_fallback_keys": [],
            "next_batch_token": "s1",
        }),
    );
    let events = decrypted_to_device["ok"]["to_device_events"].as_array().unwrap();
    let key_event = events
        .iter()
        .find(|e| e["type"] == json!("m.room_key"))
        .expect("agent should return the decrypted room key");
    // The machine stores the key internally, but room keys are
    // zeroized when returned to the client (they must not leak into
    // client-visible JSON or logs).
    assert!(
        !key_event["content"]["session_id"].as_str().unwrap().is_empty(),
        "room key session id should be present"
    );
    assert_eq!(
        key_event["content"]["session_key"],
        json!(""),
        "room keys must be zeroized in processed to-device events"
    );

    // Alice encrypts an event.
    let content = json!({"body": "It's a secret to everybody.", "msgtype": "m.text"});
    let content_raw_value =
        serde_json::value::RawValue::from_string(content.to_string()).unwrap();
    let content_raw: ruma::serde::Raw<ruma::events::AnyMessageLikeEventContent> =
        ruma::serde::Raw::from_json(content_raw_value);
    let encrypted = alice
        .encrypt_room_event_raw(room_id, "m.room.message", &content_raw)
        .await
        .unwrap();
    json!({
        "sender": "@alice:example.org",
        "type": "m.room.encrypted",
        "origin_server_ts": 0,
        "event_id": "$fake",
        "room_id": room_id,
        "content": serde_json::to_value(encrypted.content).unwrap(),
    })
}

#[test]
fn test_hello() {
    let mut agent = TestAgent::spawn();
    let response = agent.request("hello", json!({}));
    assert_eq!(response["ok"]["protocol_version"], json!(1));
}

/// Respond to the agent's outgoing requests as a virtual homeserver.
/// `alice_keys`/`alice_otk` are Alice's published key uploads (used to
/// answer keys/query and keys/claim); returns the to-device messages
/// the agent wanted sent, as raw events for Alice's machine.
fn pump_agent(agent: &mut TestAgent, alice_keys: &Value, alice_otk: &Value) -> Vec<Value> {
    let mut sent_to_device = Vec::new();
    // The agent machine always tracks its own user, so keys/query
    // responses must also include its own device keys, or it will
    // re-query forever.
    let mut bob_device_keys = None;
    loop {
        let outgoing = agent.request("outgoing_requests", json!({}));
        let requests = outgoing["ok"]["requests"].as_array().unwrap().clone();
        if requests.is_empty() {
            break;
        }
        for request in requests {
            let path = request["path"].as_str().unwrap().to_owned();
            let response = if path.contains("/keys/upload") {
                // Report the number of one-time keys the server now
                // holds, else the machine keeps generating and
                // uploading more of them.
                let body: Value =
                    serde_json::from_str(request["body"].as_str().unwrap()).unwrap();
                bob_device_keys = Some(body["device_keys"].clone());
                let otk_count = body["one_time_keys"]
                    .as_object()
                    .map(|keys| keys.len())
                    .unwrap_or(0);
                json!({"one_time_key_counts": {"signed_curve25519": otk_count}})
            } else if path.contains("/keys/query") {
                let mut device_keys = json!({
                    "@alice:example.org": {"ALICEDEVICE": alice_keys},
                });
                if let Some(bob) = &bob_device_keys {
                    device_keys["@bob:example.org"] = json!({"BOBDEVICE": bob});
                }
                json!({"device_keys": device_keys, "failures": {}})
            } else if path.contains("/keys/claim") {
                let key_id = alice_otk
                    .as_object()
                    .unwrap()
                    .keys()
                    .next()
                    .unwrap()
                    .clone();
                let key_value = alice_otk.as_object().unwrap().values().next().unwrap().clone();
                json!({
                    "one_time_keys": {
                        "@alice:example.org": {"ALICEDEVICE": {key_id: key_value}},
                    },
                    "failures": {},
                })
            } else if path.contains("/sendToDevice") {
                let body: Value = serde_json::from_str(request["body"].as_str().unwrap()).unwrap();
                for user_messages in body["messages"].as_object().unwrap().values() {
                    if let Some(devices) = user_messages.as_object() {
                        for content in devices.values() {
                            sent_to_device.push(json!({
                                "sender": "@bob:example.org",
                                "type": "m.room.encrypted",
                                "content": content,
                            }));
                        }
                    }
                }
                json!({})
            } else {
                panic!("unexpected outgoing request path: {path}")
            };
            let mark = agent.request(
                "mark_request_as_sent",
                json!({"request_id": request["id"], "response": response}),
            );
            if mark.get("err").is_some() {
                eprintln!("pump: mark FAILED for {path}: {mark}");
            }
        }
    }
    sent_to_device
}

/// The agent encrypts an event for a room whose other member is Alice
/// (an in-process machine), and Alice decrypts it.  Exercises the
/// full E2 send flow: tracking, keys/claim dance, room key sharing,
/// and encryption, all through the protocol layer.
#[tokio::test]
async fn test_agent_encrypts_event() {
    // Alice publishes her keys through the harness.
    let alice = OlmMachine::new(
        ruma::user_id!("@alice:example.org"),
        ruma::device_id!("ALICEDEVICE"),
    )
    .await;
    let mut alice_keys = None;
    let mut alice_otk = None;
    for request in alice.outgoing_requests().await.unwrap() {
        if let AnyOutgoingRequest::KeysUpload(upload) = request.request() {
            alice_keys = Some(serde_json::to_value(&upload.device_keys).unwrap());
            alice_otk = Some(serde_json::to_value(&upload.one_time_keys).unwrap());
            alice
                .mark_request_as_sent(
                    request.request_id(),
                    &UploadKeysResponse::new(BTreeMap::new()),
                )
                .await
                .unwrap();
        }
    }
    let alice_keys = alice_keys.expect("alice should upload keys");
    let alice_otk = alice_otk.expect("alice should upload one-time keys");

    // The agent (Bob) tracks Alice and tries to encrypt.
    let mut agent = TestAgent::spawn();
    let store = TempDir::new().unwrap();
    let initialize = agent.request("initialize", initialize_params(&store));
    assert!(initialize["ok"].is_object());
    let bob_sender_key = initialize["ok"]["identity_keys"]["curve25519"]
        .as_str()
        .unwrap()
        .to_owned();
    agent.request(
        "update_tracked_users",
        json!({"users": ["@alice:example.org"]}),
    );

    let encrypt_params = json!({
        "room_id": "!room:example.org",
        "event_type": "m.room.message",
        "content": {"msgtype": "m.text", "body": "from the agent"},
        "users": ["@alice:example.org"],
    });
    // Pump first: the keys/query for Alice's devices (queued by
    // update_tracked_users) must complete before encrypting, otherwise
    // the key would be shared with nobody.
    let _sent = pump_agent(&mut agent, &alice_keys, &alice_otk);
    let response = agent.request("encrypt_room_event", encrypt_params.clone());
    // First encrypt attempt: Olm sessions with Alice's device are missing.
    assert_eq!(
        response["ok"]["status"],
        json!("claims_pending"),
        "expected claims_pending: {response}"
    );

    // Perform the pending requests (claim + others).
    let _sent = pump_agent(&mut agent, &alice_keys, &alice_otk);

    // Retry: now the agent can share a room key and encrypt.
    let response = agent.request("encrypt_room_event", encrypt_params.clone());
    let event = &response["ok"]["event"];
    assert_eq!(
        event["type"],
        json!("m.room.encrypted"),
        "expected an encrypted event: {response}"
    );

    // Perform the key-share to-device requests and deliver them to
    // Alice's machine.
    let sent = pump_agent(&mut agent, &alice_keys, &alice_otk);
    assert!(
        !sent.is_empty(),
        "the agent should share the room key with Alice's device"
    );
    let room_key_events: Vec<_> = sent
        .iter()
        .map(raw_from_value::<AnyToDeviceEvent>)
        .collect();
    alice
        .receive_sync_changes(
            EncryptionSyncChanges {
                to_device_events: room_key_events,
                changed_devices: &Default::default(),
                one_time_keys_counts: &Default::default(),
                unused_fallback_keys: None,
                next_batch_token: None,
            },
            &DecryptionSettings {
                sender_device_trust_requirement: TrustRequirement::Untrusted,
            },
        )
        .await
        .unwrap();

    // Alice decrypts the event the agent produced.
    let decrypted = alice
        .decrypt_room_event(
            &raw_from_value::<EncryptedEvent>(
                &json!({
                    "sender": "@bob:example.org",
                    "sender_key": bob_sender_key,
                    "type": "m.room.encrypted",
                    "event_id": "$fake",
                    "room_id": "!room:example.org",
                    "origin_server_ts": 0,
                    "content": event["content"].clone(),
                }),
            ),
            ruma::room_id!("!room:example.org"),
            &DecryptionSettings {
                sender_device_trust_requirement: TrustRequirement::Untrusted,
            },
        )
        .await
        .expect("alice should decrypt the agent's event");
    assert_eq!(
        serde_json::to_value(decrypted.event).unwrap()["content"]["body"],
        json!("from the agent")
    );
}

#[test]
fn test_framing_error() {
    let mut agent = TestAgent::spawn();
    // Malformed JSON.
    agent.send(&json!("{oops"));
    let mut line = String::new();
    agent.stdout.read_line(&mut line).expect("read response");
    let response: Value = serde_json::from_str(&line).unwrap();
    assert_eq!(response["err"]["code"], json!("parse"));
    // Valid JSON that is not an object.
    agent.send(&json!("not an object"));
    let mut line = String::new();
    agent.stdout.read_line(&mut line).expect("read response");
    let response: Value = serde_json::from_str(&line).unwrap();
    assert_eq!(response["err"]["code"], json!("parse"));
}

#[test]
fn test_unknown_command() {
    let mut agent = TestAgent::spawn();
    let response = agent.request("frobnicate", json!({}));
    assert_eq!(response["err"]["code"], json!("unknown_command"));
}

#[test]
fn test_initialize() {
    let mut agent = TestAgent::spawn();
    let store = TempDir::new().unwrap();
    let response = agent.request("initialize", initialize_params(&store));
    let ok = &response["ok"];
    assert!(ok["identity_keys"]["curve25519"].is_string());
    assert!(ok["identity_keys"]["ed25519"].is_string());
    assert_eq!(ok["user_id"], json!("@bob:example.org"));
}

/// Device keys and signatures are only fetched by a /keys/query.  The
/// tracked-users set is persisted, so after a restart nothing would
/// re-query it (signature uploads don't trigger device list changes)
/// and the devices command would report a stale, typically
/// all-unverified trust view.  Initialize must therefore mark the
/// tracked users (including the machine's own user) dirty, so the
/// first outgoing_requests pump fetches fresh keys.
#[test]
fn test_initialize_refreshes_keys_queries() {
    let mut agent = TestAgent::spawn();
    let store = TempDir::new().unwrap();
    agent.request("initialize", initialize_params(&store));
    let outgoing = agent.request("outgoing_requests", json!({}));
    let requests = outgoing["ok"]["requests"].as_array().unwrap();
    let has_keys_query = requests
        .iter()
        .any(|request| request["path"] == json!("/_matrix/client/v3/keys/query"));
    assert!(has_keys_query, "expected a keys/query after initialize");
}

/// The elisp side re-encodes sync data with `json-serialize', which
/// encodes absent sync fields (nil) as empty objects ({}), not null.
/// The agent must accept both forms for all optional sync fields.
#[tokio::test]
async fn test_receive_sync_changes_accepts_empty_objects() {
    let mut agent = TestAgent::spawn();
    let store = TempDir::new().unwrap();
    let initialize = agent.request("initialize", initialize_params(&store));
    assert!(initialize["ok"].is_object());

    let response = agent.request(
        "receive_sync_changes",
        json!({
            "to_device_events": [],
            "changed_devices": {},
            "one_time_keys_count": {},
            "unused_fallback_keys": {},
            "next_batch_token": "s1",
        }),
    );
    assert!(
        response["ok"].is_object(),
        "empty objects must be accepted: {response}"
    );

    let response = agent.request(
        "receive_sync_changes",
        json!({
            "to_device_events": [],
            "changed_devices": null,
            "one_time_keys_count": null,
            "unused_fallback_keys": null,
            "next_batch_token": "s2",
        }),
    );
    assert!(
        response["ok"].is_object(),
        "nulls must be accepted: {response}"
    );
}

/// The flagship round trip: Alice shares a room key with Bob (the
/// agent), then sends an encrypted event which the agent decrypts.
/// The test harness acts as the virtual homeserver.
#[tokio::test]
async fn test_alice_bob_round_trip() {
    let alice = OlmMachine::new(ruma::user_id!("@alice:example.org"), ruma::device_id!("ALICEDEVICE")).await;
    let mut agent = TestAgent::spawn();
    let store = TempDir::new().unwrap();
    let initialize = agent.request("initialize", initialize_params(&store));
    assert!(initialize["ok"].is_object());

    let encrypted_event = exchange_and_encrypt(&mut agent, &alice).await;
    let decrypted = agent.request(
        "decrypt_room_event",
        json!({"room_id": "!room:example.org", "event": encrypted_event}),
    );
    assert_eq!(
        decrypted["ok"]["event"]["content"]["body"],
        json!("It's a secret to everybody."),
    );
    // The shield state (the sdk's recommended decoration) is always
    // present: "None", or a Red/Grey object with a code and message.
    let shield = &decrypted["ok"]["shield"];
    assert!(
        shield == "None" || shield["Red"]["code"].is_string() || shield["Grey"]["code"].is_string(),
        "shield state must be None or a Red/Grey object: {shield}"
    );
}

/// Sessions and identity keys must survive an agent restart: after
/// the key exchange with one agent process, a fresh agent process
/// with the same store decrypts the same event without receiving the
/// room key again.
#[tokio::test]
async fn test_restart_persistence() {
    let alice = OlmMachine::new(ruma::user_id!("@alice:example.org"), ruma::device_id!("ALICEDEVICE")).await;
    let store = TempDir::new().unwrap();
    let encrypted_event;
    let identity_keys;
    {
        let mut agent = TestAgent::spawn();
        let initialize = agent.request("initialize", initialize_params(&store));
        identity_keys = initialize["ok"]["identity_keys"].clone();
        encrypted_event = exchange_and_encrypt(&mut agent, &alice).await;
        let quit = agent.request("quit", json!({}));
        assert_eq!(quit["ok"]["bye"], json!(true));
    }

    // A fresh agent process reopens the same store.
    let mut agent = TestAgent::spawn();
    let initialize = agent.request("initialize", initialize_params(&store));
    assert_eq!(
        initialize["ok"]["identity_keys"],
        identity_keys,
        "identity keys must be stable across restarts"
    );
    let decrypted = agent.request(
        "decrypt_room_event",
        json!({"room_id": "!room:example.org", "event": encrypted_event}),
    );
    assert_eq!(
        decrypted["ok"]["event"]["content"]["body"],
        json!("It's a secret to everybody."),
        "sessions must survive an agent restart"
    );
}

/// A store opened with a different device ID than the one it was
/// created for must be refused: a device's crypto identity may not be
/// reused under a different device.
#[tokio::test]
async fn test_initialize_rejects_device_mismatch() {
    let store = TempDir::new().unwrap();
    {
        let mut agent = TestAgent::spawn();
        let initialize = agent.request("initialize", initialize_params(&store));
        assert!(initialize["ok"].is_object());
        agent.request("quit", json!({}));
    }
    let mut agent = TestAgent::spawn();
    let mut params = initialize_params(&store);
    params["device_id"] = json!("OTHERDEVICE");
    let initialize = agent.request("initialize", params);
    assert!(initialize["err"].is_object(), "mismatch must fail: {initialize}");
    assert!(
        initialize["err"]["message"]
            .as_str()
            .unwrap()
            .contains("doesn't match the account in the constructor"),
        "error should mention the account mismatch: {initialize}"
    );
}

/// A per-device store path nested under a legacy (user-level) store
/// directory inherits the legacy database when it belongs to the same
/// device (identity keys are preserved).
#[tokio::test]
async fn test_initialize_migrates_legacy_store() {
    let root = TempDir::new().unwrap();
    let legacy = root.path().join("_bob_example.org");
    let per_device = legacy.join("BOBDEVICE");
    let identity_keys;
    {
        let mut agent = TestAgent::spawn();
        let initialize = agent.request(
            "initialize",
            json!({"user_id": "@bob:example.org", "device_id": "BOBDEVICE",
                   "store_path": legacy.to_str().unwrap()}),
        );
        identity_keys = initialize["ok"]["identity_keys"].clone();
        agent.request("quit", json!({}));
    }
    // Reopen with the per-device path: the legacy database is moved.
    let mut agent = TestAgent::spawn();
    let initialize = agent.request(
        "initialize",
        json!({"user_id": "@bob:example.org", "device_id": "BOBDEVICE",
               "store_path": per_device.to_str().unwrap()}),
    );
    assert_eq!(
        initialize["ok"]["identity_keys"],
        identity_keys,
        "identity keys must survive the migration"
    );
    agent.request("quit", json!({}));
    assert!(
        !legacy.join("matrix-sdk-crypto.sqlite3").exists(),
        "the legacy database must have been moved"
    );
    assert!(per_device.join("matrix-sdk-crypto.sqlite3").exists());
}

/// A legacy store belonging to a DIFFERENT device is left alone: the
/// new device gets a fresh store (a device's crypto identity may not
/// be inherited from another device).
#[tokio::test]
async fn test_initialize_does_not_inherit_foreign_legacy_store() {
    let root = TempDir::new().unwrap();
    let legacy = root.path().join("_bob_example.org");
    {
        let mut agent = TestAgent::spawn();
        let initialize = agent.request(
            "initialize",
            json!({"user_id": "@bob:example.org", "device_id": "BOBDEVICE",
                   "store_path": legacy.to_str().unwrap()}),
        );
        assert!(initialize["ok"].is_object());
        agent.request("quit", json!({}));
    }
    // A fresh login on the same account gets a new device ID: its
    // store must NOT inherit the old device's keys.
    let per_device = legacy.join("NEWDEVICE");
    let mut agent = TestAgent::spawn();
    let initialize = agent.request(
        "initialize",
        json!({"user_id": "@bob:example.org", "device_id": "NEWDEVICE",
               "store_path": per_device.to_str().unwrap()}),
    );
    assert!(initialize["ok"].is_object(), "{initialize}");
    let identity_keys = initialize["ok"]["identity_keys"].clone();
    agent.request("quit", json!({}));
    // The legacy database is untouched...
    assert!(legacy.join("matrix-sdk-crypto.sqlite3").exists());
    // ...and the new device has its own fresh identity.
    let mut agent = TestAgent::spawn();
    let initialize = agent.request(
        "initialize",
        json!({"user_id": "@bob:example.org", "device_id": "NEWDEVICE",
               "store_path": per_device.to_str().unwrap()}),
    );
    assert_eq!(initialize["ok"]["identity_keys"], identity_keys);
}

/// E4: device A creates a backup, backs up a room key it received,
/// stores the backup key as an SSSS secret; device B (fresh store)
/// restores the backup with the SSSS recovery key and decrypts the
/// same event.
#[tokio::test]
async fn test_backup_restore_round_trip() {
    let store_a = TempDir::new().unwrap();
    let mut agent_a = TestAgent::spawn();
    assert!(
        agent_a
            .request(
                "initialize",
                json!({"user_id": "@bob:example.org", "device_id": "BOBDEVICE",
                       "store_path": store_a.path().to_str().unwrap()})
            )["ok"]
            .is_object()
    );

    let alice =
        OlmMachine::new(ruma::user_id!("@alice:example.org"), ruma::device_id!("ALICEDEVICE")).await;
    let encrypted_event = exchange_and_encrypt(&mut agent_a, &alice).await;
    // The agent must have decrypted (and thus hold) the room key.
    agent_a
        .request(
            "decrypt_room_event",
            json!({"room_id": "!room:example.org", "event": encrypted_event}),
        );

    // Setup: create the backup and enable it.
    let created = agent_a.request("backup_create", json!({}));
    let recovery_key = created["ok"]["recovery_key"].as_str().unwrap().to_owned();
    let auth_data = created["ok"]["auth_data"].clone();
    assert_eq!(
        created["ok"]["algorithm"],
        json!("m.megolm_backup.v1.curve25519-aes-sha2")
    );
    assert!(auth_data["public_key"].as_str().is_some());
    agent_a.request(
        "backup_enable",
        json!({"recovery_key": recovery_key, "version": "1"}),
    );
    // The machine can export the decryption key it has saved.
    let exported = agent_a.request("backup_recovery_key", json!({}));
    assert_eq!(
        exported["ok"]["recovery_key"],
        json!(recovery_key),
        "{exported}"
    );

    // Back up the room key: one request, then nothing more.
    let backup = agent_a.request("backup_room_keys", json!({}));
    let request = &backup["ok"]["request"];
    assert!(request["id"].as_str().is_some(), "{backup}");
    assert_eq!(
        request["path"],
        json!("/_matrix/client/v3/room_keys/keys"),
        "{backup}"
    );
    assert_eq!(request["params"]["version"], json!("1"), "{backup}");
    let uploaded: Value =
        serde_json::from_str(request["body"].as_str().unwrap()).unwrap();
    assert!(
        uploaded["rooms"]["!room:example.org"]["sessions"].is_object(),
        "{uploaded}"
    );
    agent_a
        .request("backup_mark_as_sent", json!({"id": request["id"]}));
    let after = agent_a.request("backup_status", json!({}));
    assert_eq!(
        after["ok"]["room_key_counts"]["backed_up"],
        after["ok"]["room_key_counts"]["total"],
        "all keys backed up: {after}"
    );
    assert_eq!(after["ok"]["version"], json!("1"));

    // SSSS: store the backup recovery key as a secret.
    let ssss = agent_a.request("ssss_create", json!({}));
    let key_id = ssss["ok"]["key_id"].as_str().unwrap().to_owned();
    let ssss_recovery = ssss["ok"]["recovery_key"].as_str().unwrap().to_owned();
    assert!(!ssss_recovery.is_empty());
    assert_eq!(
        ssss["ok"]["content"]["algorithm"],
        json!("m.secret_storage.v1.aes-hmac-sha2")
    );
    let secret_b64 = base64_encode(recovery_key.as_bytes());
    let encrypted = agent_a.request(
        "ssss_encrypt_secret",
        json!({"key_id": key_id, "recovery_key": ssss_recovery,
               "key_content": ssss["ok"]["content"],
               "name": "m.megolm_backup.v1", "secret": secret_b64}),
    );

    // The recovery key must verify against the key content...
    let checked = agent_a.request(
        "ssss_check_key",
        json!({"key_id": key_id, "recovery_key": ssss_recovery,
               "key_content": ssss["ok"]["content"]}),
    );
    assert_eq!(checked["ok"]["valid"], json!(true));
    // ...and a wrong key must be rejected.
    let rejected = agent_a.request(
        "ssss_check_key",
        json!({"key_id": key_id, "recovery_key": "EsAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
               "key_content": ssss["ok"]["content"]}),
    );
    assert!(rejected["err"].is_object(), "{rejected}");

    // Restore: a fresh device of the same user with the SSSS
    // recovery key.
    let store_b = TempDir::new().unwrap();
    let mut agent_b = TestAgent::spawn();
    assert!(
        agent_b
            .request(
                "initialize",
                json!({"user_id": "@bob:example.org", "device_id": "BOBDEVICE2",
                       "store_path": store_b.path().to_str().unwrap()})
            )["ok"]
            .is_object()
    );
    // On the new device the recovery key verifies before use, and no
    // backup decryption key is saved yet.
    let checked_b = agent_b.request(
        "ssss_check_key",
        json!({"key_id": key_id, "recovery_key": ssss_recovery,
               "key_content": ssss["ok"]["content"]}),
    );
    assert_eq!(checked_b["ok"]["valid"], json!(true));
    let none_saved = agent_b.request("backup_recovery_key", json!({}));
    assert!(none_saved["ok"]["recovery_key"].is_null(), "{none_saved}");
    let decrypted_secret = agent_b.request(
        "ssss_decrypt_secret",
        json!({"key_id": key_id, "recovery_key": ssss_recovery,
               "key_content": ssss["ok"]["content"],
               "name": "m.megolm_backup.v1",
               "iv": encrypted["ok"]["iv"],
               "ciphertext": encrypted["ok"]["ciphertext"],
               "mac": encrypted["ok"]["mac"]}),
    );
    assert_eq!(
        decrypted_secret["ok"]["secret"],
        json!(secret_b64),
        "the secret must survive the SSSS round trip: {decrypted_secret}"
    );
    let backup_recovery = String::from_utf8(
        base64_decode(decrypted_secret["ok"]["secret"].as_str().unwrap()),
    )
    .unwrap();

    // The restored backup key must match the backup's auth data...
    let verified = agent_b.request(
        "backup_verify",
        json!({"recovery_key": backup_recovery,
               "backup_info": {"algorithm": "m.megolm_backup.v1.curve25519-aes-sha2",
                               "auth_data": auth_data}}),
    );
    assert_eq!(verified["ok"]["matches"], json!(true));
    // ...and after enabling it, the downloaded backup decrypts.
    agent_b.request(
        "backup_enable",
        json!({"recovery_key": backup_recovery, "version": "1"}),
    );
    let exported_b = agent_b.request("backup_recovery_key", json!({}));
    assert_eq!(
        exported_b["ok"]["recovery_key"],
        json!(backup_recovery),
        "{exported_b}"
    );
    let rooms = uploaded["rooms"].clone();
    let imported =
        agent_b.request("backup_import", json!({"recovery_key": backup_recovery, "rooms": rooms}));
    assert_eq!(
        imported["ok"]["imported"],
        json!(1),
        "one room key imported: {imported}"
    );
    // Null rooms is elisp's encoding of an absent/empty backup
    // (leman-e2ee--encode): it is tolerated and imports nothing.
    let empty = agent_b.request(
        "backup_import",
        json!({"recovery_key": backup_recovery, "rooms": null}),
    );
    assert_eq!(empty["ok"]["imported"], json!(0), "{empty}");
    let decrypted = agent_b.request(
        "decrypt_room_event",
        json!({"room_id": "!room:example.org", "event": encrypted_event}),
    );
    assert_eq!(
        decrypted["ok"]["event"]["content"]["body"],
        json!("It's a secret to everybody."),
        "the restored device decrypts old history"
    );
    agent_a.request("quit", json!({}));
    agent_b.request("quit", json!({}));
}

/// E4 follow-up: keys export/import in the Element key-export file
/// format: one agent exports its keys, a fresh agent imports them
/// and decrypts old history.
#[tokio::test]
async fn test_key_export_import() {
    let store_a = TempDir::new().unwrap();
    let mut agent_a = TestAgent::spawn();
    assert!(
        agent_a
            .request(
                "initialize",
                json!({"user_id": "@bob:example.org", "device_id": "BOBDEVICE",
                       "store_path": store_a.path().to_str().unwrap()})
            )["ok"]
            .is_object()
    );

    let alice =
        OlmMachine::new(ruma::user_id!("@alice:example.org"), ruma::device_id!("ALICEDEVICE")).await;
    let encrypted_event = exchange_and_encrypt(&mut agent_a, &alice).await;
    agent_a
        .request(
            "decrypt_room_event",
            json!({"room_id": "!room:example.org", "event": encrypted_event}),
        );

    // Export on agent A.
    let exported = agent_a.request(
        "export_room_keys",
        json!({"passphrase": "hunter2"}),
    );
    let keys = exported["ok"]["keys"].as_str().unwrap();
    assert!(keys.starts_with("-----BEGIN MEGOLM SESSION DATA-----"), "{keys}");

    // Import on a fresh agent B, which then decrypts old history.
    let store_b = TempDir::new().unwrap();
    let mut agent_b = TestAgent::spawn();
    assert!(
        agent_b
            .request(
                "initialize",
                json!({"user_id": "@bob:example.org", "device_id": "BOBDEVICE2",
                       "store_path": store_b.path().to_str().unwrap()})
            )["ok"]
            .is_object()
    );
    let imported = agent_b.request(
        "import_room_keys",
        json!({"keys": keys, "passphrase": "hunter2"}),
    );
    assert_eq!(
        imported["ok"]["imported"],
        json!(1),
        "one room key imported: {imported}"
    );
    // A wrong passphrase must fail.
    let wrong = agent_b.request(
        "import_room_keys",
        json!({"keys": keys, "passphrase": "wrong"}),
    );
    assert!(wrong["err"].is_object(), "{wrong}");
    let decrypted = agent_b.request(
        "decrypt_room_event",
        json!({"room_id": "!room:example.org", "event": encrypted_event}),
    );
    assert_eq!(
        decrypted["ok"]["event"]["content"]["body"],
        json!("It's a secret to everybody."),
        "the importing device decrypts old history"
    );
    agent_a.request("quit", json!({}));
    agent_b.request("quit", json!({}));
}


fn base64_encode(data: &[u8]) -> String {
    use std::io::Write;
    let mut out = Vec::new();
    {
        let mut encoder =
            base64::write::EncoderWriter::new(&mut out, &base64::engine::general_purpose::STANDARD);
        encoder.write_all(data).unwrap();
    }
    String::from_utf8(out).unwrap()
}

fn base64_decode(data: &str) -> Vec<u8> {
    use base64::Engine as _;
    base64::engine::general_purpose::STANDARD.decode(data).unwrap()
}

/// The current time as a to-device event origin_server_ts (the fake
/// homeserver stamps events like a real one would).
fn now_ts() -> Value {    json!(std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_millis() as u64)
}

/// Convert an outgoing verification request's to-device messages into
/// raw to-device events (as the homeserver would deliver them).
fn verification_to_device_events(
    request: &OutgoingVerificationRequest,
    sender: &ruma::UserId,
) -> Vec<Value> {
    let mut events = Vec::new();
    if let OutgoingVerificationRequest::ToDevice(to_device) = request {
        for devices in to_device.messages.values() {
            for _device in devices.keys() {
                events.push(json!({
                    "sender": sender,
                    "type": serde_json::to_value(&to_device.event_type).unwrap(),
                    "origin_server_ts": now_ts(),
                    "content": serde_json::to_value(
                        devices.values().next().unwrap(),
                    )
                    .unwrap(),
                }));
            }
        }
    }
    events
}

/// Deliver to-device events to the agent.
async fn feed_agent(agent: &mut TestAgent, events: Vec<Value>) {
    if events.is_empty() {
        return;
    }
    let events: Vec<Value> = events
        .into_iter()
        .map(|mut event| {
            // Stamp events that only carry bare content.
            if event.get("origin_server_ts").is_none() {
                event["origin_server_ts"] = now_ts();
            }
            event
        })
        .collect();
    let response = agent.request(
        "receive_sync_changes",
        json!({
            "to_device_events": events,
            "changed_devices": null,
            "one_time_keys_count": null,
            "unused_fallback_keys": null,
            "next_batch_token": null,
        }),
    );
    assert!(response["ok"].is_object(), "feed_agent: {response}");
}

/// Perform the agent's outgoing requests against the virtual
/// homeserver, delivering to-device messages to ALICE's machine
/// (in-process, same user) and answering keys/query with both
/// devices' keys.  Returns when no outgoing requests remain.
async fn exchange_verification_traffic(
    agent: &mut TestAgent,
    alice: &OlmMachine,
    alice_device_keys: &Value,
    agent_device_keys: &Value,
) {
    use ruma::api::client::keys::get_keys::v3::Response as GetKeysResponse;
    use ruma::{device_id, serde::Raw, user_id};

    for _round in 0..20 {
        let mut quiet = true;

        // Agent -> Alice.
        let outgoing = agent.request("outgoing_requests", json!({}));
        let requests = outgoing["ok"]["requests"].as_array().unwrap().clone();
        if !requests.is_empty() {
            quiet = false;
        }
        let mut for_alice = Vec::new();
        for request in requests {
            let path = request["path"].as_str().unwrap().to_owned();
            if path.contains("/keys/upload") {
                let body: Value =
                    serde_json::from_str(request["body"].as_str().unwrap()).unwrap();
                let otk_count =
                    body["one_time_keys"].as_object().map(|k| k.len()).unwrap_or(0);
                agent.request(
                    "mark_request_as_sent",
                    json!({"request_id": request["id"],
                           "response": {"one_time_key_counts": {"signed_curve25519": otk_count}}}),
                );
            } else if path.contains("/keys/query") {
                agent.request(
                    "mark_request_as_sent",
                    json!({"request_id": request["id"], "response": {
                        "device_keys": {"@bob:example.org": {
                            "BOBDEVICE": agent_device_keys,
                            "ALICEDEVICE": alice_device_keys,
                        }},
                        "failures": {},
                    }}),
                );
            } else if path.contains("/keys/claim") {
                // Alice has no one-time keys uploaded in this test's
                // scenario beyond her initial upload; answer from the
                // initial upload passed via the closure is not
                // available here, so answer with an empty map (the
                // verification flow does not need to claim keys for
                // self-verification with both keys known).
                agent.request(
                    "mark_request_as_sent",
                    json!({"request_id": request["id"], "response": {
                        "one_time_keys": {}, "failures": {},
                    }}),
                );
            } else if path.contains("/upload_signatures") {
                agent.request(
                    "mark_request_as_sent",
                    json!({"request_id": request["id"], "response": {"failures": {}}}),
                );
            } else if path.contains("/sendToDevice") {
                let body: Value =
                    serde_json::from_str(request["body"].as_str().unwrap()).unwrap();
                let event_type = path
                    .split('/')
                    .nth(5)
                    .expect("event type in sendToDevice path")
                    .to_owned();
                if let Some(messages) = body["messages"].as_object() {
                    for (_user, devices) in messages {
                        if let Some(devices) = devices.as_object() {
                            for content in devices.values() {
                                for_alice.push(json!({
                                    "sender": "@bob:example.org",
                                    "type": event_type,
                                    "origin_server_ts": now_ts(),
                                    "content": content,
                                }));
                            }
                        }
                    }
                }
                agent.request(
                    "mark_request_as_sent",
                    json!({"request_id": request["id"], "response": {}}),
                );
            } else {
                panic!("unexpected outgoing request path: {path}");
            }
        }
        if !for_alice.is_empty() {
            let events = for_alice
                .iter()
                .map(raw_from_value::<AnyToDeviceEvent>)
                .collect();
            alice
                .receive_sync_changes(
                    EncryptionSyncChanges {
                        to_device_events: events,
                        changed_devices: &Default::default(),
                        one_time_keys_counts: &Default::default(),
                        unused_fallback_keys: None,
                        next_batch_token: None,
                    },
                    &DecryptionSettings {
                        sender_device_trust_requirement: TrustRequirement::Untrusted,
                    },
                )
                .await
                .unwrap();
        }

        // Alice -> Agent.
        let mut for_agent = Vec::new();
        for request in alice.outgoing_requests().await.unwrap() {
            quiet = false;
            match request.request() {
                AnyOutgoingRequest::KeysUpload(_) => {
                    alice
                        .mark_request_as_sent(
                            request.request_id(),
                            &UploadKeysResponse::new(BTreeMap::new()),
                        )
                        .await
                        .unwrap();
                }
                AnyOutgoingRequest::KeysQuery(_) => {
                    let mut response = GetKeysResponse::new();
                    response.device_keys.insert(
                        user_id!("@bob:example.org").to_owned(),
                        {
                            let mut devices = BTreeMap::new();
                            devices.insert(
                                device_id!("ALICEDEVICE").to_owned(),
                                serde_json::from_value::<Raw<ruma::encryption::DeviceKeys>>(
                                    alice_device_keys.clone(),
                                )
                                .unwrap(),
                            );
                            devices.insert(
                                device_id!("BOBDEVICE").to_owned(),
                                serde_json::from_value::<Raw<ruma::encryption::DeviceKeys>>(
                                    agent_device_keys.clone(),
                                )
                                .unwrap(),
                            );
                            devices
                        },
                    );
                    alice
                        .mark_request_as_sent(request.request_id(), &response)
                        .await
                        .unwrap();
                }
                AnyOutgoingRequest::ToDeviceRequest(to_device) => {
                    for devices in to_device.messages.values() {
                        for content in devices.values() {
                            for_agent.push(json!({
                                "sender": "@bob:example.org",
                                "type": serde_json::to_value(&to_device.event_type).unwrap(),
                                "content": serde_json::to_value(content).unwrap(),
                            }));
                        }
                    }
                    alice
                        .mark_request_as_sent(
                            request.request_id(),
                            &ruma::api::client::to_device::send_event_to_device::v3::Response::new(),
                        )
                        .await
                        .unwrap();
                }
                AnyOutgoingRequest::SignatureUpload(_) => {
                    alice
                        .mark_request_as_sent(
                            request.request_id(),
                            &ruma::api::client::keys::upload_signatures::v3::Response::new(),
                        )
                        .await
                        .unwrap();
                }
                other => panic!("unexpected alice outgoing request: {other:?}"),
            }
        }
        feed_agent(agent, for_agent).await;

        if quiet {
            break;
        }
    }
}

/// The full SAS verification dance: an in-process machine (Alice
/// device of the agent's own user) verifies the agent's device
/// through the protocol, exactly as Element would.
#[tokio::test]
async fn test_verification_sas_round_trip() {
    // The agent (bob @BOBDEVICE).
    let mut agent = TestAgent::spawn();
    let store = TempDir::new().unwrap();
    let initialize = agent.request("initialize", initialize_params(&store));
    assert!(initialize["ok"].is_object());
    // Alice: another device of the same user.
    let alice =
        OlmMachine::new(ruma::user_id!("@bob:example.org"), ruma::device_id!("ALICEDEVICE")).await;
    let mut alice_device_keys = None;
    for request in alice.outgoing_requests().await.unwrap() {
        if let AnyOutgoingRequest::KeysUpload(upload) = request.request() {
            alice_device_keys = Some(serde_json::to_value(&upload.device_keys).unwrap());
            alice
                .mark_request_as_sent(
                    request.request_id(),
                    &UploadKeysResponse::new(BTreeMap::new()),
                )
                .await
                .unwrap();
        }
    }
    let alice_device_keys = alice_device_keys.expect("alice uploads keys");

    // Capture the agent's device keys from its initial key upload and
    // mark it as sent (the virtual homeserver keeps them).
    let mut agent_device_keys = None;
    let outgoing = agent.request("outgoing_requests", json!({}));
    for request in outgoing["ok"]["requests"].as_array().unwrap() {
        let path = request["path"].as_str().unwrap().to_owned();
        if path.contains("/keys/upload") {
            let body: Value =
                serde_json::from_str(request["body"].as_str().unwrap()).unwrap();
            agent_device_keys = Some(body["device_keys"].clone());
            let otk_count = body["one_time_keys"].as_object().map(|k| k.len()).unwrap_or(0);
            agent.request(
                "mark_request_as_sent",
                json!({"request_id": request["id"],
                       "response": {"one_time_key_counts": {"signed_curve25519": otk_count}}}),
            );
        } else if path.contains("/keys/query") {
            agent.request(
                "mark_request_as_sent",
                json!({"request_id": request["id"], "response": {
                    "device_keys": {"@bob:example.org": {
                        "ALICEDEVICE": alice_device_keys,
                    }},
                    "failures": {},
                }}),
            );
        }
    }
    let agent_device_keys = agent_device_keys.expect("agent uploads keys");

    // Exchange device keys both ways so each machine knows the other.
    exchange_verification_traffic(
        &mut agent,
        &alice,
        &alice_device_keys,
        &agent_device_keys,
    )
    .await;

    // Alice requests verification of the agent's device.
    let bob_device = alice
        .get_device(
            ruma::user_id!("@bob:example.org"),
            ruma::device_id!("BOBDEVICE"),
            None,
        )
        .await
        .unwrap()
        .expect("alice should know the agent's device after a key query");
    let (_request, outgoing) = bob_device.request_verification();
    let events = verification_to_device_events(&outgoing, ruma::user_id!("@bob:example.org"));
    feed_agent(&mut agent, events).await;

    // The agent sees the incoming request.
    let requests = agent.request("verification_requests", json!({}));
    let flow = {
        let list = requests["ok"]["requests"].as_array().unwrap();
        assert_eq!(list.len(), 1, "agent should see the request: {requests}");
        assert_eq!(list[0]["state"], json!("created"));
        assert_eq!(list[0]["we_started"], json!(false));
        list[0]["flow_id"].as_str().unwrap().to_owned()
    };

    // The agent accepts; traffic flows both ways.
    agent.request(
        "accept_verification",
        json!({"user_id": "@bob:example.org", "flow_id": flow}),
    );
    exchange_verification_traffic(
        &mut agent,
        &alice,
        &alice_device_keys,
        &agent_device_keys,
    )
    .await;

    // Alice starts the SAS.
    let request = alice
        .get_verification_request(ruma::user_id!("@bob:example.org"), &flow)
        .expect("alice keeps the request");

    let (sas, start) = request.start_sas().await.unwrap().expect("alice starts sas");
    let events = verification_to_device_events(&start, ruma::user_id!("@bob:example.org"));
    feed_agent(&mut agent, events).await;

    // The agent has a SAS object; it accepts their start.
    let requests = agent.request("verification_requests", json!({}));
    assert_eq!(requests["ok"]["requests"][0]["sas"], json!(true));
    agent.request(
        "accept_sas",
        json!({"user_id": "@bob:example.org", "flow_id": flow}),
    );
    exchange_verification_traffic(
        &mut agent,
        &alice,
        &alice_device_keys,
        &agent_device_keys,
    )
    .await;

    // The agent can present the emoji.
    let sas_state = agent.request(
        "verification_sas",
        json!({"user_id": "@bob:example.org", "flow_id": flow}),
    );
    assert_eq!(
        sas_state["ok"]["can_be_presented"],
        json!(true),
        "emoji should be presentable: {sas_state}"
    );
    let emoji = sas_state["ok"]["emoji"].as_array().unwrap();
    assert_eq!(emoji.len(), 7);
    assert!(!emoji[0]["symbol"].as_str().unwrap().is_empty());
    assert!(emoji[0]["number"].as_u64().is_some());

    // Both sides confirm the short auth string.
    agent.request(
        "confirm_sas",
        json!({"user_id": "@bob:example.org", "flow_id": flow}),
    );
    exchange_verification_traffic(
        &mut agent,
        &alice,
        &alice_device_keys,
        &agent_device_keys,
    )
    .await;
    let (mac_requests, _signature) = sas.confirm().await.unwrap();
    // The caller must perform the MAC requests returned by confirm.
    // (They are not queued in the machine's outgoing requests.)
    for outgoing in mac_requests {
        let events = verification_to_device_events(&outgoing, ruma::user_id!("@bob:example.org"));
        feed_agent(&mut agent, events).await;
    }
    exchange_verification_traffic(
        &mut agent,
        &alice,
        &alice_device_keys,
        &agent_device_keys,
    )
    .await;

    // Done: the agent's device is verified from Alice's perspective,
    // and Alice's device is verified from the agent's.
    let requests = agent.request("verification_requests", json!({}));
    assert_eq!(requests["ok"]["requests"][0]["state"], json!("done"));
    let bob_device = alice
        .get_device(
            ruma::user_id!("@bob:example.org"),
            ruma::device_id!("BOBDEVICE"),
            None,
        )
        .await
        .unwrap()
        .unwrap();
    assert!(
        bob_device.is_verified(),
        "the agent's device should be verified after the dance"
    );
    let devices = agent.request(
        "devices",
        json!({"user_id": "@bob:example.org"}),
    );
    let listed = devices["ok"]["devices"]
        .as_array()
        .unwrap()
        .iter()
        .find(|d| d["device_id"] == json!("ALICEDEVICE"))
        .expect("the agent should list alice's device");
    assert_eq!(listed["verified"], json!(true));
}
