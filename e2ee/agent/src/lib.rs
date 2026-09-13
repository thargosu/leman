//! Leman E2EE agent: a JSON-line protocol over stdio wrapping the
//! matrix-sdk-crypto state machine.  See e2ee/PROTOCOL.org.

use std::collections::BTreeMap;

use anyhow::{anyhow, Context, Result};
use matrix_sdk_common::deserialized_responses::ProcessedToDeviceEvent;
use matrix_sdk_crypto::{
    secret_storage::{AesHmacSha2EncryptedData, SecretStorageKey},
    store::types::BackupDecryptionKey,
    types::events::room::encrypted::EncryptedEvent,
    types::requests::{AnyOutgoingRequest, OutgoingVerificationRequest},
    DecryptionSettings, EncryptionSettings, EncryptionSyncChanges, OlmMachine,
    TrustRequirement,
};
use ruma::{
    api::client as client_api,
    events::{
        secret::request::SecretName,
        secret_storage::key::{
            SecretStorageEncryptionAlgorithm, SecretStorageKeyEventContent,
            SecretStorageV1AesHmacSha2Properties,
        },
        AnyToDeviceEvent, MessageLikeEventContent,
    },
    serde::Raw,
    OneTimeKeyAlgorithm, OwnedDeviceId, OwnedTransactionId, OwnedUserId, UInt,
};
use serde_json::{json, Value};

/// What the main loop should do after handling a line.
pub enum Flow {
    /// Send this (possibly empty) response and keep running.
    Respond(String),
    /// Send this response and exit.
    Quit(String),
}

/// Error codes for the protocol's `err` responses.
enum AgentError {
    UnknownCommand(String),
    NotInitialized,
    Crypto(anyhow::Error),
}

impl AgentError {
    fn code(&self) -> &'static str {
        match self {
            Self::UnknownCommand(_) => "unknown_command",
            Self::NotInitialized => "not_initialized",
            Self::Crypto(_) => "crypto",
        }
    }
}

impl std::fmt::Display for AgentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownCommand(command) => write!(f, "unknown command: {command}"),
            Self::NotInitialized => write!(f, "not initialized"),
            Self::Crypto(error) => write!(f, "{error:#}"),
        }
    }
}

type CommandResult = Result<Value, AgentError>;

impl From<anyhow::Error> for AgentError {
    fn from(error: anyhow::Error) -> Self {
        Self::Crypto(error)
    }
}

/// The kind of a pending outgoing request, remembered so the response
/// can be reconstructed when the client reports it as sent.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PendingKind {
    KeysUpload,
    KeysQuery,
    KeysClaim,
    ToDevice,
    SignatureUpload,
    RoomMessage,
}

/// The agent: a single OlmMachine plus bookkeeping for the protocol.
pub struct Agent {
    machine: Option<OlmMachine>,
    pending: BTreeMap<String, PendingKind>,
    /// Outgoing requests stashed by commands like `encrypt_room_event`
    /// (key claims and room key shares), merged into the next
    /// `outgoing_requests` response.
    extra_requests: Vec<Value>,
}

impl Default for Agent {
    fn default() -> Self {
        Self::new()
    }
}

impl Agent {
    pub fn new() -> Self {
        Self {
            machine: None,
            pending: BTreeMap::new(),
            extra_requests: Vec::new(),
        }
    }

    fn machine(&self) -> Result<&OlmMachine, AgentError> {
        self.machine.as_ref().ok_or(AgentError::NotInitialized)
    }

    /// Handle one protocol line.
    pub async fn handle_line(&mut self, line: &str) -> Result<Flow, anyhow::Error> {
        if line.trim().is_empty() {
            return Ok(Flow::Respond(String::new()));
        }
        let message: Value = match serde_json::from_str::<Value>(line) {
            Ok(value) if value.is_object() => value,
            Ok(_) => {
                return Ok(Flow::Respond(
                    json!({"err": {"code": "parse", "message": "protocol messages must be JSON objects"}})
                        .to_string(),
                ))
            }
            Err(error) => {
                return Ok(Flow::Respond(
                    json!({"err": {"code": "parse", "message": error.to_string()}}).to_string(),
                ))
            }
        };
        let id = message.get("id").cloned().unwrap_or(Value::Null);
        let command = message
            .get("cmd")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_owned();
        let params = message.get("params").cloned().unwrap_or_else(|| json!({}));

        if command == "quit" {
            return Ok(Flow::Quit(json!({"id": id, "ok": {"bye": true}}).to_string()));
        }

        let response = match self.handle_command(&command, params).await {
            Ok(ok) => json!({"id": id, "ok": ok}),
            Err(error) => {
                json!({"id": id, "err": {"code": error.code(), "message": error.to_string()}})
            }
        };
        Ok(Flow::Respond(response.to_string()))
    }

    async fn handle_command(&mut self, command: &str, params: Value) -> CommandResult {
        match command {
            "hello" => Ok(json!({"protocol_version": 1})),
            "initialize" => self.initialize(params).await,
            "outgoing_requests" => self.outgoing_requests().await,
            "mark_request_as_sent" => self.mark_request_as_sent(params).await,
            "receive_sync_changes" => self.receive_sync_changes(params).await,
            "decrypt_room_event" => self.decrypt_room_event(params).await,
            "update_tracked_users" => self.update_tracked_users(params).await,
            "encrypt_room_event" => self.encrypt_room_event(params).await,
            "devices" => self.devices(params).await,
            "request_verification" => self.request_verification(params).await,
            "verification_requests" => self.verification_requests(params).await,
            "accept_verification" => self.accept_verification(params).await,
            "start_sas" => self.start_sas(params).await,
            "verification_sas" => self.verification_sas(params).await,
            "accept_sas" => self.accept_sas(params).await,
            "confirm_sas" => self.confirm_sas(params).await,
            "cancel_verification" => self.cancel_verification(params).await,
            "backup_create" => self.backup_create().await,
            "backup_enable" => self.backup_enable(params).await,
            "backup_verify" => self.backup_verify(params).await,
            "backup_status" => self.backup_status().await,
            "backup_recovery_key" => self.backup_recovery_key().await,
            "backup_room_keys" => self.backup_room_keys().await,
            "backup_mark_as_sent" => self.backup_mark_as_sent(params).await,
            "backup_import" => self.backup_import(params).await,
            "ssss_create" => self.ssss_create().await,
            "ssss_encrypt_secret" => self.ssss_encrypt_secret(params).await,
            "ssss_decrypt_secret" => self.ssss_decrypt_secret(params).await,
            "ssss_check_key" => self.ssss_check_key(params).await,
            other => Err(AgentError::UnknownCommand(other.to_owned())),
        }
    }

    async fn initialize(&mut self, params: Value) -> CommandResult {
        let user_id: OwnedUserId = param_str(&params, "user_id")?
            .parse()
            .map_err(crypto_error)?;
        let device_id = OwnedDeviceId::from(param_str(&params, "device_id")?);
        let store_path = param_str(&params, "store_path")?;
        Self::migrate_legacy_store(store_path, &user_id, &device_id).await?;
        tokio::fs::create_dir_all(store_path)
            .await
            .context("creating store directory")?;
        let store = matrix_sdk_sqlite::SqliteCryptoStore::open(store_path, None)
            .await
            .context("opening crypto store")
            .map_err(crypto_error)?;
        let machine = OlmMachine::with_store(&user_id, &device_id, store, None)
            .await
            .map_err(crypto_error)?;
        let identity_keys = machine.identity_keys();
        let response = json!({
            "user_id": user_id,
            "device_id": device_id,
            "identity_keys": {
                "curve25519": identity_keys.curve25519.to_base64(),
                "ed25519": identity_keys.ed25519.to_base64(),
            },
        });
        self.machine = Some(machine);
        Ok(response)
    }

    /// Move a legacy (user-level) crypto store into its per-device
    /// path when it belongs to the same user and device.  A legacy
    /// store belonging to a DIFFERENT device is left alone: a
    /// device's crypto identity may not be inherited by another
    /// device (e.g. after a fresh login on a fresh device ID).
    async fn migrate_legacy_store(
        store_path: &str,
        user_id: &ruma::UserId,
        device_id: &ruma::DeviceId,
    ) -> anyhow::Result<()> {
        let path = std::path::Path::new(store_path);
        let Some(parent) = path.parent() else {
            return Ok(());
        };
        let legacy_db = parent.join("matrix-sdk-crypto.sqlite3");
        let target_db = path.join("matrix-sdk-crypto.sqlite3");
        if !legacy_db.exists() || target_db.exists() {
            return Ok(());
        }
        let parent_str = parent
            .to_str()
            .ok_or_else(|| anyhow!("non-UTF-8 legacy store path"))?
            .to_owned();
        let store = matrix_sdk_sqlite::SqliteCryptoStore::open(parent_str, None)
            .await
            .context("opening legacy crypto store")?;
        use matrix_sdk_crypto::store::CryptoStore as _;
        let migratable = match store.load_account().await? {
            Some(account) => {
                account.user_id() == user_id && account.device_id() == device_id
            }
            None => false,
        };
        drop(store);
        if !migratable {
            return Ok(());
        }
        std::fs::create_dir_all(path).context("creating per-device store directory")?;
        for entry in std::fs::read_dir(parent).context("reading legacy store directory")? {
            let entry = entry.context("reading legacy store directory entry")?;
            let file_name = entry.file_name();
            let name = file_name.to_string_lossy();
            if name.starts_with("matrix-sdk-crypto.sqlite3") {
                std::fs::rename(entry.path(), path.join(&*file_name))
                    .context("moving legacy crypto store file")?;
            }
        }
        Ok(())
    }

    async fn outgoing_requests(&mut self) -> CommandResult {
        let machine = self.machine()?;
        let requests = machine
            .outgoing_requests()
            .await
            .map_err(crypto_error)?;
        let mut serialized = Vec::new();
        for request in requests {
            let request_id = request.request_id().to_string();            let (kind, method, path, body) = match request.request() {
                AnyOutgoingRequest::KeysUpload(request) => (
                    PendingKind::KeysUpload,
                    "POST".to_owned(),
                    "/_matrix/client/v3/keys/upload".to_owned(),
                    json!({
                        "device_keys": request.device_keys,
                        "one_time_keys": request.one_time_keys,
                        "fallback_keys": request.fallback_keys,
                    }),
                ),
                AnyOutgoingRequest::KeysClaim(request) => (
                    PendingKind::KeysClaim,
                    "POST".to_owned(),
                    "/_matrix/client/v3/keys/claim".to_owned(),
                    json!({
                        "one_time_keys": request.one_time_keys,
                        "timeout": request.timeout.map(|timeout| timeout.as_millis() as u64),
                    }),
                ),
                AnyOutgoingRequest::SignatureUpload(request) => (
                    PendingKind::SignatureUpload,
                    "POST".to_owned(),
                    "/_matrix/client/v3/keys/upload_signatures".to_owned(),
                    json!({"signed_keys": request.signed_keys}),
                ),
                AnyOutgoingRequest::KeysQuery(request) => (
                    PendingKind::KeysQuery,
                    "POST".to_owned(),
                    "/_matrix/client/v3/keys/query".to_owned(),
                    json!({
                        "device_keys": request.device_keys,
                        "timeout": request.timeout.map(|timeout| timeout.as_millis() as u64),
                    }),
                ),
                AnyOutgoingRequest::ToDeviceRequest(request) => (
                    PendingKind::ToDevice,
                    "PUT".to_owned(),
                    format!(
                        "/_matrix/client/v3/sendToDevice/{}/{}",
                        request.event_type, request.txn_id
                    ),
                    json!({"messages": request.messages}),
                ),
                AnyOutgoingRequest::RoomMessage(request) => (
                    PendingKind::RoomMessage,
                    "PUT".to_owned(),
                    format!(
                        "/_matrix/client/v3/rooms/{}/send/{}/{}",
                        request.room_id,
                        request.content.event_type(),
                        request.txn_id
                    ),
                    serde_json::to_value(request.content.as_ref())
                        .context("serializing message content")
                        .map_err(crypto_error)?,
                ),
            };
            self.pending.insert(request_id.clone(), kind);
            // NOTE: The body is a JSON string, not an embedded object:
            // elisp cannot encode empty objects (nil becomes either
            // null or {} depending on the encoder), so round-tripping
            // a serialized body through elisp would corrupt it (e.g.
            // "timeout":null becoming "timeout":{}, which homeservers
            // reject).  The client passes the string through verbatim.
            let body = serde_json::to_string(&body)
                .context("serializing request body")
                .map_err(crypto_error)?;
            serialized.push(json!({
                "id": request_id,
                "method": method,
                "path": path,
                "body": body,
            }));
        }
        // Include requests stashed by commands like `encrypt_room_event`
        // or the verification dance.  Skip ones the machine also queued
        // (same transaction id) so they are not performed twice.
        let seen: std::collections::HashSet<String> =
            serialized.iter().map(|entry| entry["id"].as_str().unwrap_or_default().to_owned()).collect();
        for extra in self.extra_requests.drain(..) {
            let id = extra["id"].as_str().unwrap_or_default().to_owned();
            if !seen.contains(&id) {
                serialized.push(extra);
            }
        }
        Ok(json!({"requests": serialized}))
    }

    async fn mark_request_as_sent(&mut self, params: Value) -> CommandResult {
        let machine = self.machine()?;
        let request_id = param_str(&params, "request_id")?.to_owned();
        let response = params.get("response").cloned().unwrap_or(json!({}));
        let txn_id = OwnedTransactionId::from(request_id.clone());
        let kind = *self
            .pending
            .get(&request_id)
            .ok_or_else(|| AgentError::Crypto(anyhow!("unknown request {request_id}")))?;

        let result = match kind {
            PendingKind::KeysUpload => {
                let mut counts: BTreeMap<OneTimeKeyAlgorithm, UInt> = BTreeMap::new();
                if let Some(map) =
                    response.get("one_time_key_counts").and_then(Value::as_object)
                {
                    for (key, value) in map {
                        let algorithm = serde_json::from_str::<OneTimeKeyAlgorithm>(key).ok();
                        let count = value.as_u64().and_then(|count| UInt::try_from(count).ok());
                        if let (Some(algorithm), Some(count)) = (algorithm, count) {
                            counts.insert(algorithm, count);
                        }
                    }
                }
                let typed = client_api::keys::upload_keys::v3::Response::new(counts);
                machine.mark_request_as_sent(&txn_id, &typed).await
            }
            PendingKind::KeysQuery => {
                let mut typed = client_api::keys::get_keys::v3::Response::new();
                if let Some(device_keys) = response.get("device_keys") {
                    typed.device_keys = serde_json::from_value(device_keys.clone())
                        .context("deserializing device_keys")
                        .map_err(crypto_error)?;
                }
                if let Some(failures) = response.get("failures") {
                    typed.failures = serde_json::from_value(failures.clone())
                        .context("deserializing failures")
                        .map_err(crypto_error)?;
                }
                machine.mark_request_as_sent(&txn_id, &typed).await
            }
            PendingKind::KeysClaim => {
                let one_time_keys = serde_json::from_value(
                    response.get("one_time_keys").cloned().unwrap_or(json!({})),
                )
                .context("deserializing one_time_keys")
                .map_err(crypto_error)?;
                let typed = client_api::keys::claim_keys::v3::Response::new(one_time_keys);
                machine.mark_request_as_sent(&txn_id, &typed).await
            }
            PendingKind::ToDevice => {
                let typed = client_api::to_device::send_event_to_device::v3::Response::new();
                machine.mark_request_as_sent(&txn_id, &typed).await
            }
            PendingKind::SignatureUpload => {
                let typed = client_api::keys::upload_signatures::v3::Response::new();
                machine.mark_request_as_sent(&txn_id, &typed).await
            }
            PendingKind::RoomMessage => {
                let event_id: ruma::OwnedEventId = response
                    .get("event_id")
                    .and_then(Value::as_str)
                    .unwrap_or("$invalid")
                    .parse()
                    .map_err(crypto_error)?;
                let typed = client_api::message::send_message_event::v3::Response::new(event_id);
                machine.mark_request_as_sent(&txn_id, &typed).await
            }
        };
        result.map_err(crypto_error)?;
        self.pending.remove(&request_id);
        Ok(json!({}))
    }

    async fn receive_sync_changes(&mut self, params: Value) -> CommandResult {
        let machine = self.machine()?;
        let to_device_events: Vec<Raw<AnyToDeviceEvent>> = params
            .get("to_device_events")
            .and_then(Value::as_array)
            .ok_or_else(|| AgentError::Crypto(anyhow!("missing to_device_events")))?
            .iter()
            .map(raw_from_value)
            .collect::<Result<Vec<_>, _>>()
            .map_err(crypto_error)?;
        // NOTE: Clients may omit these sync fields (or send null;
        // elisp has no empty-object representation), so treat null
        // as absent.
        let changed_devices: ruma::api::client::sync::sync_events::DeviceLists =
            match params.get("changed_devices") {
                Some(Value::Null) | None => Default::default(),
                Some(value) => serde_json::from_value(value.clone())
                    .context("parsing changed_devices")
                    .map_err(crypto_error)?,
            };
        let one_time_keys_count: BTreeMap<OneTimeKeyAlgorithm, UInt> =
            match params.get("one_time_keys_count") {
                Some(Value::Null) | None => Default::default(),
                Some(value) => serde_json::from_value(value.clone())
                    .context("parsing one_time_keys_count")
                    .map_err(crypto_error)?,
            };
        let unused_fallback_keys: Option<Vec<OneTimeKeyAlgorithm>> = match params
            .get("unused_fallback_keys")
        {
            // NOTE: The client sends an empty object ({}), not null,
            // for absent sync fields; elisp cannot encode null
            // objects.
            Some(Value::Null) | None => None,
            Some(Value::Object(object)) if object.is_empty() => None,
            Some(value) => serde_json::from_value(value.clone())
                .with_context(|| format!("parsing unused_fallback_keys: {value}"))
                .map_err(crypto_error)?,
        };
        let next_batch_token: Option<String> = params
            .get("next_batch_token")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned);

        let changes = EncryptionSyncChanges {
            to_device_events,
            changed_devices: &changed_devices,
            one_time_keys_counts: &one_time_keys_count,
            unused_fallback_keys: unused_fallback_keys.as_deref(),
            next_batch_token,
        };
        let settings = DecryptionSettings {
            sender_device_trust_requirement: TrustRequirement::Untrusted,
        };
        let (processed, _room_key_infos) = machine
            .receive_sync_changes(changes, &settings)
            .await
            .map_err(crypto_error)?;
        let events = processed
            .iter()
            .map(|event| match event {
                ProcessedToDeviceEvent::Decrypted { raw, .. } => serde_json::to_value(raw),
                ProcessedToDeviceEvent::PlainText(raw)
                | ProcessedToDeviceEvent::Invalid(raw) => serde_json::to_value(raw),
                ProcessedToDeviceEvent::UnableToDecrypt { encrypted_event, .. } => {
                    serde_json::to_value(encrypted_event)
                }
            })
            .collect::<Result<Vec<_>, _>>()
            .context("serializing processed events")
            .map_err(crypto_error)?;
        let outgoing = self.outgoing_requests().await?;
        Ok(json!({
            "to_device_events": events,
            "outgoing_requests": outgoing["requests"],
        }))
    }

    async fn decrypt_room_event(&mut self, params: Value) -> CommandResult {
        let machine = self.machine()?;
        let room_id = param_str(&params, "room_id")?
            .parse::<ruma::OwnedRoomId>()
            .map_err(crypto_error)?;
        let event: Value = params
            .get("event")
            .cloned()
            .ok_or_else(|| AgentError::Crypto(anyhow!("missing event")))?;
        let event: Raw<EncryptedEvent> = raw_from_value(&event).map_err(crypto_error)?;
        let settings = DecryptionSettings {
            sender_device_trust_requirement: TrustRequirement::Untrusted,
        };
        let decrypted = machine
            .decrypt_room_event(&event, &room_id, &settings)
            .await
            .map_err(crypto_error)?;
        let event = serde_json::to_value(&decrypted.event)
            .context("serializing decrypted event")
            .map_err(crypto_error)?;
        Ok(json!({"event": event}))
    }

    async fn update_tracked_users(&mut self, params: Value) -> CommandResult {
        let machine = self.machine()?;
        let users: Vec<OwnedUserId> = params
            .get("users")
            .and_then(Value::as_array)
            .ok_or_else(|| AgentError::Crypto(anyhow!("missing users")))?
            .iter()
            .map(|user| {
                user.as_str()
                    .ok_or_else(|| AgentError::Crypto(anyhow!("invalid user")))?
                    .parse::<OwnedUserId>()
                    .map_err(crypto_error)
            })
            .collect::<Result<Vec<_>, _>>()?;
        machine
            .update_tracked_users(users.iter().map(|user| user.as_ref()))
            .await
            .map_err(crypto_error)?;
        Ok(json!({}))
    }

    /// Encrypt an event's content with Megolm (E2).  The room key is
    /// shared with the given members' devices first; any Olm sessions
    /// that are still missing cause a `claims_pending` response, with
    /// the keys/claim request stashed for the client to perform
    /// before retrying.
    async fn encrypt_room_event(&mut self, params: Value) -> CommandResult {
        let machine = self.machine()?;
        let room_id: ruma::OwnedRoomId = param_str(&params, "room_id")?
            .parse()
            .map_err(crypto_error)?;
        let event_type = param_str(&params, "event_type")?.to_owned();
        let content = params
            .get("content")
            .cloned()
            .ok_or_else(|| AgentError::Crypto(anyhow!("missing param \"content\"")))?;
        let users: Vec<ruma::OwnedUserId> = params
            .get("users")
            .and_then(Value::as_array)
            .ok_or_else(|| AgentError::Crypto(anyhow!("missing param \"users\"")))?
            .iter()
            .map(|user| {
                user.as_str()
                    .ok_or_else(|| AgentError::Crypto(anyhow!("invalid user")))?
                    .parse()
                    .map_err(crypto_error)
            })
            .collect::<Result<Vec<_>, _>>()?;
        if users.is_empty() {
            // Refuse: the room key would be shared with nobody, and
            // the event could never be decrypted by other members.
            return Err(AgentError::Crypto(anyhow!("empty users")));
        }

        // Claim one-time keys for devices we have no Olm session with.
        if let Some((txn_id, claim_request)) = machine
            .get_missing_sessions(users.iter().map(|user| user.as_ref()))
            .await
            .map_err(crypto_error)?
        {
            let request_id = txn_id.as_str().to_owned();
            let body = serde_json::to_string(&json!({
                "one_time_keys": claim_request.one_time_keys,
                "timeout": claim_request.timeout
                    .map(|timeout| timeout.as_millis() as u64),
            }))
            .context("serializing keys/claim body")
            .map_err(crypto_error)?;
            let entry = json!({
                "id": request_id,
                "method": "POST",
                "path": "/_matrix/client/v3/keys/claim",
                "body": body,
            });
            self.pending.insert(request_id, PendingKind::KeysClaim);
            self.extra_requests.push(entry);
            return Ok(json!({"status": "claims_pending"}));
        }

        // Share the room key with the members' devices.  The share
        // requests are stashed as outgoing requests; other clients
        // cannot decrypt until the client has performed them.
        let share_requests = machine
            .share_room_key(
                &room_id,
                users.iter().map(|user| user.as_ref()),
                EncryptionSettings::default(),
            )
            .await
            .map_err(crypto_error)?;
        let mut stashed = Vec::new();
        for send in share_requests {
            let request_id = send.txn_id.as_str().to_owned();
            let body = serde_json::to_string(&json!({"messages": send.messages}))
                .context("serializing sendToDevice body")
                .map_err(crypto_error)?;
            let path = format!(
                "/_matrix/client/v3/sendToDevice/{}/{}",
                send.event_type, send.txn_id
            );
            stashed.push((request_id, path, body));
        }

        let content_raw_value = serde_json::value::RawValue::from_string(content.to_string())
            .context("serializing content")
            .map_err(crypto_error)?;
        let content_raw = Raw::from_json(content_raw_value);
        let encrypted = machine
            .encrypt_room_event_raw(&room_id, &event_type, &content_raw)
            .await
            .map_err(crypto_error)?;
        let encrypted_content =
            serde_json::to_value(&encrypted.content)
                .context("serializing encrypted content")
                .map_err(crypto_error)?;

        // The machine is not needed anymore; stash the share requests.
        for (request_id, path, body) in stashed {
            self.pending.insert(request_id.clone(), PendingKind::ToDevice);
            self.extra_requests.push(json!({
                "id": request_id,
                "method": "PUT",
                "path": path,
                "body": body,
            }));
        }
        Ok(json!({
            "status": "ok",
            "event": {"type": "m.room.encrypted", "content": encrypted_content},
        }))
    }

    /// List a user's known devices (E3), with their verification state.
    async fn devices(&mut self, params: Value) -> CommandResult {
        let machine = self.machine()?;
        let user_id: ruma::OwnedUserId = match params.get("user_id").and_then(Value::as_str) {
            Some(user) => user.parse().map_err(crypto_error)?,
            None => machine.user_id().to_owned(),
        };
        let devices = machine
            .get_user_devices(&user_id, None)
            .await
            .map_err(crypto_error)?;
        let list = devices
            .devices()
            .map(|device| {
                json!({
                    "device_id": device.device_id(),
                    "display_name": device.display_name(),
                    "verified": device.is_verified(),
                    "deleted": device.is_deleted(),
                })
            })
            .collect::<Vec<_>>();
        Ok(json!({"devices": list}))
    }

    /// Start verifying a device (E3).  The initial
    /// m.key.verification.request is returned directly by the state
    /// machine (not queued in its outgoing requests), so it must be
    /// stashed here.
    async fn request_verification(&mut self, params: Value) -> CommandResult {
        let machine = self.machine()?;
        let user_id: ruma::OwnedUserId = param_str(&params, "user_id")?.parse().map_err(crypto_error)?;
        let device_id = ruma::OwnedDeviceId::from(param_str(&params, "device_id")?);
        let device = machine
            .get_device(&user_id, &device_id, None)
            .await
            .map_err(crypto_error)?
            .ok_or_else(|| AgentError::Crypto(anyhow!("unknown device {device_id}")))?;
        let (request, outgoing) = device.request_verification();
        let flow_id = request.flow_id().as_str().to_owned();
        self.stash_outgoing_verification(outgoing);
        Ok(json!({"flow_id": flow_id}))
    }

    /// List the known verification requests for a user (the machine's
    /// own user by default), including whether a SAS object exists.
    async fn verification_requests(&mut self, params: Value) -> CommandResult {
        let machine = self.machine()?;
        let user_id: ruma::OwnedUserId = match params.get("user_id").and_then(Value::as_str) {
            Some(user) => user.parse().map_err(crypto_error)?,
            None => machine.user_id().to_owned(),
        };
        let requests = machine
            .get_verification_requests(&user_id)
            .into_iter()
            .map(|request| {
                let state = if request.is_done() {
                    "done"
                } else if request.is_cancelled() {
                    "cancelled"
                } else if request.is_ready() {
                    "ready"
                } else {
                    "created"
                };
                let flow_id = request.flow_id().as_str().to_owned();
                let sas = machine
                    .get_verification(&user_id, &flow_id)
                    .map(|verification| matches!(verification, matrix_sdk_crypto::Verification::SasV1(_)))
                    .unwrap_or(false);
                json!({
                    "flow_id": flow_id,
                    "user_id": request.other_user(),
                    "device_id": request.other_device_id(),
                    "state": state,
                    "we_started": request.we_started(),
                    "sas": sas,
                })
            })
            .collect::<Vec<_>>();
        Ok(json!({"requests": requests}))
    }

    /// Accept an incoming verification request (sends ready).
    async fn accept_verification(&mut self, params: Value) -> CommandResult {
        let (request, user_id, flow_id) = self.verification_request(&params)?;
        if let Some(outgoing) = request.accept() {
            self.stash_outgoing_verification(outgoing);
        }
        Ok(json!({"flow_id": flow_id, "user_id": user_id}))
    }

    /// Start SAS for a request (sends start), or report an existing
    /// SAS when their start already arrived.
    async fn start_sas(&mut self, params: Value) -> CommandResult {
        let (request, user_id, flow_id) = self.verification_request(&params)?;
        match request.start_sas().await.map_err(crypto_error)? {
            Some((_sas, outgoing)) => {
                self.stash_outgoing_verification(outgoing);
                Ok(json!({"flow_id": flow_id, "user_id": user_id}))
            }
            None => Ok(json!({"flow_id": flow_id, "user_id": user_id})),
        }
    }

    /// Report the SAS state of a flow (emoji, presented, done, ...).
    async fn verification_sas(&mut self, params: Value) -> CommandResult {
        let machine = self.machine()?;
        let (user_id, flow_id) = self.verification_ids(&params)?;
        let sas = self.verification_sas_for(machine, &user_id, &flow_id)?;
        let emoji = sas.emoji().map(|emojis| {
            let indices = sas.emoji_index().unwrap_or([0; 7]);
            emojis
                .iter()
                .enumerate()
                .map(|(i, emoji)| {
                    json!({
                        "number": indices[i],
                        "symbol": emoji.symbol,
                        "description": emoji.description,
                    })
                })
                .collect::<Vec<_>>()
        });
        Ok(json!({
            "accepted": sas.has_been_accepted(),
            "can_be_presented": sas.can_be_presented(),
            "done": sas.is_done(),
            "cancelled": sas.is_cancelled(),
            "emoji": emoji,
        }))
    }

    /// Accept their SAS start (sends accept).
    async fn accept_sas(&mut self, params: Value) -> CommandResult {
        let machine = self.machine()?;
        let (user_id, flow_id) = self.verification_ids(&params)?;
        let sas = self.verification_sas_for(machine, &user_id, &flow_id)?;
        if let Some(outgoing) = sas.accept() {
            self.stash_outgoing_verification(outgoing);
        }
        Ok(json!({}))
    }

    /// Confirm the short auth string (sends mac; a signature upload
    /// may follow via outgoing_requests).
    async fn confirm_sas(&mut self, params: Value) -> CommandResult {
        let machine = self.machine()?;
        let (user_id, flow_id) = self.verification_ids(&params)?;
        let sas = self.verification_sas_for(machine, &user_id, &flow_id)?;
        let (requests, signature_upload) = sas.confirm().await.map_err(crypto_error)?;
        for outgoing in requests {
            self.stash_outgoing_verification(outgoing);
        }
        if let Some(upload) = signature_upload {
            let request_id = ruma::TransactionId::new().to_string();
            let body = serde_json::to_string(&json!({"signed_keys": upload.signed_keys}))
                .context("serializing signature upload body")
                .map_err(crypto_error)?;
            self.pending.insert(request_id.clone(), PendingKind::SignatureUpload);
            self.extra_requests.push(json!({
                "id": request_id,
                "method": "POST",
                "path": "/_matrix/client/v3/keys/upload_signatures",
                "body": body,
            }));
        }
        Ok(json!({}))
    }

    /// Cancel a verification request or SAS.
    async fn cancel_verification(&mut self, params: Value) -> CommandResult {
        let (request, _user_id, _flow_id) = self.verification_request(&params)?;
        if let Some(outgoing) = request.cancel() {
            self.stash_outgoing_verification(outgoing);
        }
        Ok(json!({}))
    }

    /// Generate a fresh backup decryption key and the signed auth data
    /// for a new Megolm v1 backup version.
    async fn backup_create(&self) -> CommandResult {
        let machine = self.machine()?;

        let key = BackupDecryptionKey::new();

        let mut info = key.to_backup_info();
        machine
            .backup_machine()
            .sign_backup(&mut info)
            .await
            .map_err(crypto_error)?;
        let matrix_sdk_crypto::types::RoomKeyBackupInfo::MegolmBackupV1Curve25519AesSha2(
            auth_data,
        ) = info else {
            return Err(crypto_error("unexpected backup algorithm"));
        };

        Ok(json!({
            "recovery_key": key.to_base58(),
            "algorithm": "m.megolm_backup.v1.curve25519-aes-sha2",
            "auth_data": serde_json::to_value(&auth_data).map_err(crypto_error)?,
        }))
    }

    /// Import a backup decryption key (base58) and enable backing up
    /// future room keys for the given version.  Also used by a new
    /// device to adopt a backup for restoring.
    async fn backup_enable(&self, params: Value) -> CommandResult {
        let machine = self.machine()?;
        let recovery_key = param_str(&params, "recovery_key")?;
        let version = param_str(&params, "version")?.to_owned();

        let key = BackupDecryptionKey::from_base58(recovery_key).map_err(crypto_error)?;
        let megolm_key = key.megolm_v1_public_key();
        megolm_key.set_version(version.clone());

        let backup_machine = machine.backup_machine();
        backup_machine
            .save_decryption_key(Some(key), Some(version.clone()))
            .await
            .map_err(crypto_error)?;
        backup_machine.enable_backup_v1(megolm_key).await.map_err(crypto_error)?;
        Ok(json!({}))
    }

    /// Check whether a recovery key matches a backup (the full
    /// m.room_key.backup account-data content).
    async fn backup_verify(&self, params: Value) -> CommandResult {
        let recovery_key = param_str(&params, "recovery_key")?;
        let info: matrix_sdk_crypto::types::RoomKeyBackupInfo = serde_json::from_value(
            params.get("backup_info").cloned().unwrap_or(json!({})),
        )
        .map_err(crypto_error)?;
        let key = BackupDecryptionKey::from_base58(recovery_key).map_err(crypto_error)?;
        Ok(json!({"matches": key.backup_key_matches(&info)}))
    }

    async fn backup_status(&self) -> CommandResult {
        let machine = self.machine()?;
        let backup_machine = machine.backup_machine();
        let enabled = backup_machine.enabled().await;
        let version = backup_machine.backup_version().await;
        let counts = backup_machine.room_key_counts().await.map_err(crypto_error)?;
        Ok(json!({
            "enabled": enabled,
            "version": version,
            "room_key_counts": {"total": counts.total, "backed_up": counts.backed_up},
        }))
    }

    /// Return the backup decryption key the machine has saved (its
    /// base58 recovery key), or null when none is saved.  Lets the
    /// client re-store the key under another secret-storage key
    /// without asking the user to type it again.
    async fn backup_recovery_key(&self) -> CommandResult {
        use matrix_sdk_crypto::store::types::BackupKeys;
        let machine = self.machine()?;
        let keys: BackupKeys = machine.backup_machine().get_backup_keys().await
            .map_err(crypto_error)?;
        Ok(json!({
            "recovery_key": match keys.decryption_key {
                Some(key) => json!(key.to_base58()),
                None => Value::Null,
            }
        }))
    }

    /// Encrypt the not-yet-backed-up room keys.  The returned request
    /// is for POST /room_keys/keys/{version}; the client reports it
    /// with `backup_mark_as_sent`, which lets the machine clear its
    /// pending backup (the same request is returned until then).
    async fn backup_room_keys(&mut self) -> CommandResult {
        let machine = self.machine()?;
        let request = machine.backup_machine().backup().await.map_err(crypto_error)?;
        let Some((txn_id, keys_backup)) = request else {
            return Ok(json!({"request": Value::Null}));
        };
        let body = json!({"version": keys_backup.version, "rooms": keys_backup.rooms});
        Ok(json!({"request": {
            "id": txn_id.as_str(),
            "path": format!("/_matrix/client/v3/room_keys/keys/{}", keys_backup.version),
            "body": body.to_string(),
        }}))
    }

    async fn backup_mark_as_sent(&self, params: Value) -> CommandResult {
        let machine = self.machine()?;
        let request_id = param_str(&params, "id")?.to_owned();
        let txn_id = OwnedTransactionId::from(request_id);
        let typed = client_api::backup::add_backup_keys::v3::Response::new(
            String::new(),
            UInt::from(0u32),
        );
        machine.mark_request_as_sent(&txn_id, &typed).await.map_err(crypto_error)?;
        Ok(json!({}))
    }

    /// Import room keys downloaded with
    /// GET /room_keys/keys?version={version} (the "rooms" value of the
    /// response).  Each session's encrypted session data is decrypted
    /// with the backup decryption key, so it must already be enabled
    /// (or passed here) for the backup the keys came from.
    async fn backup_import(&self, params: Value) -> CommandResult {
        use matrix_sdk_crypto::olm::BackedUpRoomKey;
        use ruma::api::client::backup::KeyBackupData;
        let machine = self.machine()?;
        let recovery_key = param_str(&params, "recovery_key")?;
        let key = BackupDecryptionKey::from_base58(recovery_key).map_err(crypto_error)?;

        let rooms_value = params.get("rooms").cloned().unwrap_or(json!({}));
        let mut parsed: BTreeMap<ruma::OwnedRoomId, BTreeMap<String, BackedUpRoomKey>> =
            BTreeMap::new();
        let Some(rooms) = rooms_value.as_object() else {
            return Err(crypto_error("missing or invalid param \"rooms\""));
        };
        for (room_id, room) in rooms {
            let room_id: ruma::OwnedRoomId = room_id.parse().map_err(crypto_error)?;
            let mut sessions = BTreeMap::new();
            if let Some(map) = room.get("sessions").and_then(Value::as_object) {
                for (session_id, data) in map {
                    let backup_data: KeyBackupData =
                        serde_json::from_value(data.clone()).map_err(crypto_error)?;
                    let room_key = key
                        .decrypt_session_data(backup_data.session_data)
                        .map_err(crypto_error)?;
                    sessions.insert(session_id.clone(), room_key);
                }
            }
            parsed.insert(room_id, sessions);
        }

        let version = machine.backup_machine().backup_version().await;
        let exported_keys = parsed
            .into_iter()
            .flat_map(|(room_id, sessions)| {
                sessions.into_iter().map(move |(session_id, room_key)| {
                    matrix_sdk_crypto::olm::ExportedRoomKey::from_backed_up_room_key(
                        room_id.clone(),
                        session_id,
                        room_key,
                    )
                })
            })
            .collect::<Vec<_>>();
        let result = machine
            .store()
            .import_room_keys(exported_keys, version.as_deref(), |_, _| {})
            .await
            .map_err(crypto_error)?;
        Ok(json!({"imported": result.imported_count, "total": result.total_count}))
    }

    /// Generate the secret storage default key.
    async fn ssss_create(&self) -> CommandResult {
        let key = SecretStorageKey::new();
        Ok(json!({
            "key_id": key.key_id(),
            "recovery_key": key.to_base58(),
            "content": serde_json::to_value(key.event_content()).map_err(crypto_error)?,
        }))
    }

    /// Rebuild the SSSS key from the recovery key, its id and the
    /// m.secret_storage.key.<key_id> account-data content.
    fn ssss_key_from_params(&self, params: &Value) -> Result<SecretStorageKey, AgentError> {
        let recovery_key = param_str(params, "recovery_key")?;
        let key_id = param_str(params, "key_id")?.to_owned();
        let content = params
            .get("key_content")
            .cloned()
            .unwrap_or(json!({}));
        let iv: ruma::serde::Base64 = serde_json::from_value(
            content
                .get("iv")
                .cloned()
                .ok_or_else(|| crypto_error("missing key_content.iv"))?,
        )
        .map_err(crypto_error)?;
        let mac: ruma::serde::Base64 = serde_json::from_value(
            content
                .get("mac")
                .cloned()
                .ok_or_else(|| crypto_error("missing key_content.mac"))?,
        )
        .map_err(crypto_error)?;
        let content = SecretStorageKeyEventContent::new(
            key_id,
            SecretStorageEncryptionAlgorithm::V1AesHmacSha2(
                SecretStorageV1AesHmacSha2Properties::new(Some(iv), Some(mac)),
            ),
        );
        SecretStorageKey::from_account_data(recovery_key, content).map_err(crypto_error)
    }

    async fn ssss_encrypt_secret(&self, params: Value) -> CommandResult {
        let key = self.ssss_key_from_params(&params)?;
        let name: SecretName = param_str(&params, "name")?.to_owned().into();
        let secret_b64 = param_str(&params, "secret")?;
        let plaintext = base64_decode(secret_b64).map_err(crypto_error)?;
        let data = key.encrypt(plaintext, &name);
        serde_json::to_value(&data).map_err(crypto_error)
    }

    async fn ssss_decrypt_secret(&self, params: Value) -> CommandResult {
        let key = self.ssss_key_from_params(&params)?;
        let name: SecretName = param_str(&params, "name")?.to_owned().into();
        let iv = base64_decode(param_str(&params, "iv")?)
            .map_err(crypto_error)?
            .try_into()
            .map_err(|_| crypto_error("invalid IV length"))?;
        let mac = base64_decode(param_str(&params, "mac")?)
            .map_err(crypto_error)?
            .try_into()
            .map_err(|_| crypto_error("invalid MAC length"))?;
        let ciphertext = base64_decode(param_str(&params, "ciphertext")?).map_err(crypto_error)?;
        let data = AesHmacSha2EncryptedData {
            iv,
            ciphertext: ruma::serde::Base64::new(ciphertext),
            mac,
        };
        let plaintext = key.decrypt(&data, &name).map_err(crypto_error)?;
        Ok(json!({"secret": base64_encode(&plaintext)}))
    }

    /// Check whether the recovery key unlocks the SSSS key described
    /// by key_content (the zero-message MAC check).
    async fn ssss_check_key(&self, params: Value) -> CommandResult {
        self.ssss_key_from_params(&params)?;
        Ok(json!({"valid": true}))
    }

    /// Fetch the live SAS object of a flow.
    fn verification_sas_for(
        &self,
        machine: &OlmMachine,
        user_id: &ruma::UserId,
        flow_id: &str,
    ) -> Result<matrix_sdk_crypto::Sas, AgentError> {
        match machine.get_verification(user_id, flow_id) {
            Some(matrix_sdk_crypto::Verification::SasV1(sas)) => Ok(*sas),
            _ => Err(AgentError::Crypto(anyhow!(
                "no SAS verification for flow {flow_id}"
            ))),
        }
    }

    /// Parse user_id + flow_id from verification command params.
    fn verification_ids(&self, params: &Value) -> Result<(ruma::OwnedUserId, String), AgentError> {
        let user_id: ruma::OwnedUserId =
            param_str(params, "user_id")?.parse().map_err(crypto_error)?;
        let flow_id = param_str(params, "flow_id")?.to_owned();
        Ok((user_id, flow_id))
    }

    /// Fetch the verification request for user_id + flow_id params.
    fn verification_request(
        &self,
        params: &Value,
    ) -> Result<(matrix_sdk_crypto::VerificationRequest, ruma::OwnedUserId, String), AgentError>
    {
        let machine = self.machine()?;
        let (user_id, flow_id) = self.verification_ids(params)?;
        let request = machine
            .get_verification_request(&user_id, &flow_id)
            .ok_or_else(|| AgentError::Crypto(anyhow!("no verification request for flow {flow_id}")))?;
        Ok((request, user_id, flow_id))
    }

    /// Queue a returned outgoing verification request for the client
    /// to perform.  If the machine's own outgoing requests already
    /// contain it (same transaction id), the pump's dedup skips the
    /// stashed copy.
    fn stash_outgoing_verification(&mut self, outgoing: OutgoingVerificationRequest) {
        if let OutgoingVerificationRequest::ToDevice(to_device) = outgoing {
            let Ok(body) = serde_json::to_string(&json!({"messages": to_device.messages})) else {
                return;
            };
            let request_id = to_device.txn_id.as_str().to_owned();
            self.pending.insert(request_id.clone(), PendingKind::ToDevice);
            self.extra_requests.push(json!({
                "id": request_id,
                "method": "PUT",
                "path": format!(
                    "/_matrix/client/v3/sendToDevice/{}/{}",
                    to_device.event_type, to_device.txn_id
                ),
                "body": body,
            }));
        }
    }
}

fn crypto_error<E: std::fmt::Display>(error: E) -> AgentError {
    AgentError::Crypto(anyhow!("{error}"))
}

fn param_str<'a>(params: &'a Value, key: &str) -> Result<&'a str, AgentError> {
    params
        .get(key)
        .and_then(Value::as_str)
        .ok_or_else(|| AgentError::Crypto(anyhow!("missing or invalid param {key:?}")))
}

fn raw_from_value<T>(value: &Value) -> Result<Raw<T>, anyhow::Error>
where
    T: serde::de::DeserializeOwned,
{
    let raw_value = serde_json::value::RawValue::from_string(serde_json::to_string(value)?)?;
    Ok(Raw::from_json(raw_value))
}

// Base64 in the Matrix ecosystem uses the standard alphabet, unpadded
// when encoding (matching ruma's Base64 type), and accepts both padded
// and unpadded input when decoding.
fn base64_encode(data: &[u8]) -> String {
    use base64::Engine as _;
    base64::engine::general_purpose::STANDARD_NO_PAD.encode(data)
}

fn base64_decode(data: &str) -> Result<Vec<u8>, anyhow::Error> {
    use base64::{
        Engine as _,
        engine::{DecodePaddingMode, GeneralPurpose, GeneralPurposeConfig},
    };
    let engine = GeneralPurpose::new(
        &base64::alphabet::STANDARD,
        GeneralPurposeConfig::new()
            .with_decode_allow_trailing_bits(true)
            .with_decode_padding_mode(DecodePaddingMode::Indifferent),
    );
    Ok(engine.decode(data)?)
}


