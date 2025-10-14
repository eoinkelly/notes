CREATE TABLE namespaces(
	partition_id INT NOT NULL,
	id BINARY(16) NOT NULL,
	name VARCHAR(255) UNIQUE NOT NULL,
	notification_version BIGINT NOT NULL,
	
	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16) NOT NULL,
	is_global TINYINT(1) NOT NULL,
	PRIMARY KEY(partition_id, id)
);
CREATE TABLE namespace_metadata (
	partition_id INT NOT NULL,
	notification_version BIGINT NOT NULL,
	PRIMARY KEY(partition_id)
);
CREATE TABLE shards (
	shard_id INT NOT NULL,
	
	range_id BIGINT NOT NULL,
	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16) NOT NULL,
	PRIMARY KEY (shard_id)
);
CREATE TABLE executions(
	shard_id INT NOT NULL,
	namespace_id BINARY(16) NOT NULL,
	workflow_id VARCHAR(255) NOT NULL,
	run_id BINARY(16) NOT NULL,
	
	next_event_id BIGINT NOT NULL,
	last_write_version BIGINT NOT NULL,
	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16) NOT NULL,
	state MEDIUMBLOB NOT NULL,
	state_encoding VARCHAR(16) NOT NULL,
	db_record_version BIGINT NOT NULL DEFAULT 0,
	PRIMARY KEY (shard_id, namespace_id, workflow_id, run_id)
);
CREATE TABLE current_executions(
	shard_id INT NOT NULL,
	namespace_id BINARY(16) NOT NULL,
	workflow_id VARCHAR(255) NOT NULL,
	
	run_id BINARY(16) NOT NULL,
	create_request_id VARCHAR(255) NOT NULL,
	state INT NOT NULL,
	status INT NOT NULL,
	start_time TIMESTAMP NULL,
	last_write_version BIGINT NOT NULL,
	
	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16) NOT NULL DEFAULT '',
	PRIMARY KEY (shard_id, namespace_id, workflow_id)
);
CREATE TABLE buffered_events (
	shard_id INT NOT NULL,
	namespace_id BINARY(16) NOT NULL,
	workflow_id VARCHAR(255) NOT NULL,
	run_id BINARY(16) NOT NULL,
	id BIGINT AUTO_INCREMENT UNIQUE,
	
	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16) NOT NULL,
	PRIMARY KEY (shard_id, namespace_id, workflow_id, run_id, id)
);
CREATE TABLE tasks (
	range_hash INT UNSIGNED NOT NULL,
	task_queue_id VARBINARY(272) NOT NULL,
	task_id BIGINT NOT NULL,
	
	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16) NOT NULL,
	PRIMARY KEY (range_hash, task_queue_id, task_id)
);
CREATE TABLE task_queues (
	range_hash INT UNSIGNED NOT NULL,
	task_queue_id VARBINARY(272) NOT NULL,
	
	range_id BIGINT NOT NULL,
	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16) NOT NULL,
	PRIMARY KEY (range_hash, task_queue_id)
);
CREATE TABLE task_queue_user_data (
  namespace_id    BINARY(16) NOT NULL,
  task_queue_name VARCHAR(255) NOT NULL,
  data            MEDIUMBLOB NOT NULL,
  data_encoding   VARCHAR(16) NOT NULL,
  version         BIGINT NOT NULL,
  PRIMARY KEY (namespace_id, task_queue_name)
);
CREATE TABLE build_id_to_task_queue (
  namespace_id    BINARY(16) NOT NULL,
  build_id        VARCHAR(255) NOT NULL,
  task_queue_name VARCHAR(255) NOT NULL,
  PRIMARY KEY (namespace_id, build_id, task_queue_name)
);
CREATE TABLE history_immediate_tasks(
  shard_id INT NOT NULL,
  category_id INT NOT NULL,
  task_id BIGINT NOT NULL,

  data MEDIUMBLOB NOT NULL,
  data_encoding VARCHAR(16) NOT NULL,
  PRIMARY KEY (shard_id, category_id, task_id)
);
CREATE TABLE history_scheduled_tasks (
  shard_id INT NOT NULL,
  category_id INT NOT NULL,
  visibility_timestamp TIMESTAMP NOT NULL,
  task_id BIGINT NOT NULL,

  data MEDIUMBLOB NOT NULL,
  data_encoding VARCHAR(16) NOT NULL,
  PRIMARY KEY (shard_id, category_id, visibility_timestamp, task_id)
);
CREATE TABLE transfer_tasks(
	shard_id INT NOT NULL,
	task_id BIGINT NOT NULL,
	
	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16) NOT NULL,
	PRIMARY KEY (shard_id, task_id)
);
CREATE TABLE timer_tasks (
	shard_id INT NOT NULL,
	visibility_timestamp TIMESTAMP NOT NULL,
	task_id BIGINT NOT NULL,
	
	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16) NOT NULL,
	PRIMARY KEY (shard_id, visibility_timestamp, task_id)
);
CREATE TABLE replication_tasks (
	shard_id INT NOT NULL,
	task_id BIGINT NOT NULL,
	
	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16) NOT NULL,
	PRIMARY KEY (shard_id, task_id)
);
CREATE TABLE replication_tasks_dlq (
	source_cluster_name VARCHAR(255) NOT NULL,
	shard_id INT NOT NULL,
	task_id BIGINT NOT NULL,
	
	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16) NOT NULL,
	PRIMARY KEY (source_cluster_name, shard_id, task_id)
);
CREATE TABLE visibility_tasks(
	shard_id INT NOT NULL,
	task_id BIGINT NOT NULL,
	
	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16) NOT NULL,
	PRIMARY KEY (shard_id, task_id)
);
CREATE TABLE activity_info_maps (

	shard_id INT NOT NULL,
	namespace_id BINARY(16) NOT NULL,
	workflow_id VARCHAR(255) NOT NULL,
	run_id BINARY(16) NOT NULL,
	schedule_id BIGINT NOT NULL,

	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16),
	PRIMARY KEY (shard_id, namespace_id, workflow_id, run_id, schedule_id)
);
CREATE TABLE timer_info_maps (
	shard_id INT NOT NULL,
	namespace_id BINARY(16) NOT NULL,
	workflow_id VARCHAR(255) NOT NULL,
	run_id BINARY(16) NOT NULL,
	timer_id VARCHAR(255) NOT NULL,

	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16),
	PRIMARY KEY (shard_id, namespace_id, workflow_id, run_id, timer_id)
);
CREATE TABLE child_execution_info_maps (
	shard_id INT NOT NULL,
	namespace_id BINARY(16) NOT NULL,
	workflow_id VARCHAR(255) NOT NULL,
	run_id BINARY(16) NOT NULL,
	initiated_id BIGINT NOT NULL,

	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16),
	PRIMARY KEY (shard_id, namespace_id, workflow_id, run_id, initiated_id)
);
CREATE TABLE request_cancel_info_maps (
	shard_id INT NOT NULL,
	namespace_id BINARY(16) NOT NULL,
	workflow_id VARCHAR(255) NOT NULL,
	run_id BINARY(16) NOT NULL,
	initiated_id BIGINT NOT NULL,

	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16),
	PRIMARY KEY (shard_id, namespace_id, workflow_id, run_id, initiated_id)
);
CREATE TABLE signal_info_maps (
	shard_id INT NOT NULL,
	namespace_id BINARY(16) NOT NULL,
	workflow_id VARCHAR(255) NOT NULL,
	run_id BINARY(16) NOT NULL,
	initiated_id BIGINT NOT NULL,

	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16),
	PRIMARY KEY (shard_id, namespace_id, workflow_id, run_id, initiated_id)
);
CREATE TABLE signals_requested_sets (
	shard_id INT NOT NULL,
	namespace_id BINARY(16) NOT NULL,
	workflow_id VARCHAR(255) NOT NULL,
	run_id BINARY(16) NOT NULL,
	signal_id VARCHAR(255) NOT NULL,
	
	PRIMARY KEY (shard_id, namespace_id, workflow_id, run_id, signal_id)
);
CREATE TABLE chasm_node_maps (
  shard_id INT NOT NULL,
  namespace_id BINARY(16) NOT NULL,
  workflow_id VARCHAR(255) NOT NULL,
  run_id BINARY(16) NOT NULL,
  chasm_path BINARY(1536) NOT NULL,

  metadata MEDIUMBLOB NOT NULL,
  metadata_encoding VARCHAR(16),
  data MEDIUMBLOB,
  data_encoding VARCHAR(16),
  PRIMARY KEY (shard_id, namespace_id, workflow_id, run_id, chasm_path)
);
CREATE TABLE history_node (
	shard_id INT NOT NULL,
	tree_id BINARY(16) NOT NULL,
	branch_id BINARY(16) NOT NULL,
	node_id BIGINT NOT NULL,
	txn_id BIGINT NOT NULL,
	
	prev_txn_id BIGINT NOT NULL DEFAULT 0,
	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16) NOT NULL,
	PRIMARY KEY (shard_id, tree_id, branch_id, node_id, txn_id)
);
CREATE TABLE history_tree (
	shard_id INT NOT NULL,
	tree_id BINARY(16) NOT NULL,
	branch_id BINARY(16) NOT NULL,
	
	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16) NOT NULL,
	PRIMARY KEY (shard_id, tree_id, branch_id)
);
CREATE TABLE queue (
	queue_type INT NOT NULL,
	message_id BIGINT NOT NULL,
	message_payload MEDIUMBLOB NOT NULL,
	message_encoding VARCHAR(16) NOT NULL,
	PRIMARY KEY(queue_type, message_id)
);
CREATE TABLE queue_metadata (
	queue_type INT NOT NULL,
	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16) NOT NULL,
	version BIGINT NOT NULL,
	PRIMARY KEY(queue_type)
);
CREATE TABLE cluster_metadata_info (
	metadata_partition INT NOT NULL,
	cluster_name VARCHAR(255) NOT NULL,
	data MEDIUMBLOB NOT NULL,
	data_encoding VARCHAR(16) NOT NULL,
	version BIGINT NOT NULL,
	PRIMARY KEY(metadata_partition, cluster_name)
);
CREATE TABLE cluster_membership
(
	membership_partition INT NOT NULL,
	host_id BINARY(16) NOT NULL,
	rpc_address VARCHAR(128) NOT NULL,
	rpc_port SMALLINT NOT NULL,
	role TINYINT NOT NULL,
	session_start TIMESTAMP DEFAULT '1970-01-01 00:00:01',
	last_heartbeat TIMESTAMP DEFAULT '1970-01-01 00:00:01',
	record_expiry TIMESTAMP DEFAULT '1970-01-01 00:00:01',





	PRIMARY KEY (membership_partition, host_id)
);
CREATE TABLE queues (
    queue_type INT NOT NULL,
    queue_name VARCHAR(255) NOT NULL,
    metadata_payload MEDIUMBLOB NOT NULL,
    metadata_encoding VARCHAR(16) NOT NULL,
    PRIMARY KEY (queue_type, queue_name)
);
CREATE TABLE queue_messages (
    queue_type INT NOT NULL,
    queue_name VARCHAR(255) NOT NULL,
	queue_partition BIGINT NOT NULL,
    message_id BIGINT NOT NULL,
    message_payload MEDIUMBLOB NOT NULL,
    message_encoding VARCHAR(16) NOT NULL,
    PRIMARY KEY (
        queue_type,
        queue_name,
        queue_partition,
        message_id
    )
);
CREATE TABLE nexus_endpoints (
    id             BINARY(16) NOT NULL,
    data           MEDIUMBLOB NOT NULL,
    data_encoding  VARCHAR(16) NOT NULL,
    version        BIGINT NOT NULL,
    PRIMARY KEY (id)
);
CREATE TABLE nexus_endpoints_partition_status (
    id      INT NOT NULL DEFAULT 0 CHECK (id = 0),
    version BIGINT NOT NULL,
    PRIMARY KEY (id)
);
CREATE TABLE executions_visibility (
  namespace_id            CHAR(64)      NOT NULL,
  run_id                  CHAR(64)      NOT NULL,
  _version                BIGINT        NOT NULL DEFAULT 0,
  start_time              TIMESTAMP     NOT NULL,
  execution_time          TIMESTAMP     NOT NULL,
  workflow_id             VARCHAR(255)  NOT NULL,
  workflow_type_name      VARCHAR(255)  NOT NULL,
  status                  INT           NOT NULL,
  close_time              TIMESTAMP     NULL,
  history_length          BIGINT        NULL,
  history_size_bytes      BIGINT        NULL,
  execution_duration      BIGINT        NULL,
  state_transition_count  BIGINT        NULL,
  memo                    BLOB          NULL,
  encoding                VARCHAR(64)   NOT NULL,
  task_queue              VARCHAR(255)  NOT NULL DEFAULT '',
  search_attributes       TEXT          NULL,
  parent_workflow_id      VARCHAR(255)  NULL,
  parent_run_id           VARCHAR(255)  NULL,
  root_workflow_id        VARCHAR(255)  NOT NULL DEFAULT '',
  root_run_id             VARCHAR(255)  NOT NULL DEFAULT '',


  TemporalChangeVersion         TEXT          GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.TemporalChangeVersion")) STORED,
  BinaryChecksums               TEXT          GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.BinaryChecksums"))       STORED,
  BatcherUser                   VARCHAR(255)  GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.BatcherUser")),
  TemporalScheduledStartTime    TIMESTAMP     GENERATED ALWAYS AS (STRFTIME('%Y-%m-%d %H:%M:%f+00:00', JSON_EXTRACT(search_attributes, "$.TemporalScheduledStartTime"))),
  TemporalScheduledById         VARCHAR(255)  GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.TemporalScheduledById")),
  TemporalSchedulePaused        BOOLEAN       GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.TemporalSchedulePaused")),
  TemporalNamespaceDivision     VARCHAR(255)  GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.TemporalNamespaceDivision")),
  BuildIds                      TEXT          GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.BuildIds"))              STORED,
  TemporalPauseInfo             TEXT          GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.TemporalPauseInfo"))     STORED,
  TemporalWorkerDeploymentVersion VARCHAR(255)        GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.TemporalWorkerDeploymentVersion")),
  TemporalWorkflowVersioningBehavior VARCHAR(255)     GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.TemporalWorkflowVersioningBehavior")),
  TemporalWorkerDeployment        VARCHAR(255)        GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.TemporalWorkerDeployment")),


  Bool01          BOOLEAN         GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Bool01")),
  Bool02          BOOLEAN         GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Bool02")),
  Bool03          BOOLEAN         GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Bool03")),
  Datetime01      TIMESTAMP       GENERATED ALWAYS AS (STRFTIME('%Y-%m-%d %H:%M:%f+00:00', JSON_EXTRACT(search_attributes, "$.Datetime01"))),
  Datetime02      TIMESTAMP       GENERATED ALWAYS AS (STRFTIME('%Y-%m-%d %H:%M:%f+00:00', JSON_EXTRACT(search_attributes, "$.Datetime02"))),
  Datetime03      TIMESTAMP       GENERATED ALWAYS AS (STRFTIME('%Y-%m-%d %H:%M:%f+00:00', JSON_EXTRACT(search_attributes, "$.Datetime03"))),
  Double01        DECIMAL(20, 5)  GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Double01")),
  Double02        DECIMAL(20, 5)  GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Double02")),
  Double03        DECIMAL(20, 5)  GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Double03")),
  Int01           BIGINT          GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Int01")),
  Int02           BIGINT          GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Int02")),
  Int03           BIGINT          GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Int03")),
  Keyword01       VARCHAR(255)    GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Keyword01")),
  Keyword02       VARCHAR(255)    GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Keyword02")),
  Keyword03       VARCHAR(255)    GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Keyword03")),
  Keyword04       VARCHAR(255)    GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Keyword04")),
  Keyword05       VARCHAR(255)    GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Keyword05")),
  Keyword06       VARCHAR(255)    GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Keyword06")),
  Keyword07       VARCHAR(255)    GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Keyword07")),
  Keyword08       VARCHAR(255)    GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Keyword08")),
  Keyword09       VARCHAR(255)    GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Keyword09")),
  Keyword10       VARCHAR(255)    GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Keyword10")),
  Text01          TEXT            GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Text01"))        STORED,
  Text02          TEXT            GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Text02"))        STORED,
  Text03          TEXT            GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.Text03"))        STORED,
  KeywordList01   TEXT            GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.KeywordList01")) STORED,
  KeywordList02   TEXT            GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.KeywordList02")) STORED,
  KeywordList03   TEXT            GENERATED ALWAYS AS (JSON_EXTRACT(search_attributes, "$.KeywordList03")) STORED,

  PRIMARY KEY (namespace_id, run_id)
);
CREATE INDEX default_idx                ON executions_visibility (namespace_id, (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_execution_time          ON executions_visibility (namespace_id, execution_time,         (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_workflow_id             ON executions_visibility (namespace_id, workflow_id,            (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_workflow_type           ON executions_visibility (namespace_id, workflow_type_name,     (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_status                  ON executions_visibility (namespace_id, status,                 (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_history_length          ON executions_visibility (namespace_id, history_length,         (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_history_size_bytes      ON executions_visibility (namespace_id, history_size_bytes,     (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_execution_duration      ON executions_visibility (namespace_id, execution_duration,     (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_state_transition_count  ON executions_visibility (namespace_id, state_transition_count, (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_task_queue              ON executions_visibility (namespace_id, task_queue,             (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_parent_workflow_id      ON executions_visibility (namespace_id, parent_workflow_id,     (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_parent_run_id           ON executions_visibility (namespace_id, parent_run_id,          (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_root_workflow_id        ON executions_visibility (namespace_id, root_workflow_id,       (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_root_run_id             ON executions_visibility (namespace_id, root_run_id,            (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_batcher_user                  ON executions_visibility (namespace_id, BatcherUser,                (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_temporal_scheduled_start_time ON executions_visibility (namespace_id, TemporalScheduledStartTime, (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_temporal_scheduled_by_id      ON executions_visibility (namespace_id, TemporalScheduledById,      (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_temporal_schedule_paused      ON executions_visibility (namespace_id, TemporalSchedulePaused,     (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_temporal_namespace_division   ON executions_visibility (namespace_id, TemporalNamespaceDivision,  (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_temporal_pause_info           ON executions_visibility (namespace_id, TemporalPauseInfo,          (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_temporal_worker_deployment_version ON executions_visibility (namespace_id, TemporalWorkerDeploymentVersion,  (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_temporal_workflow_versioning_behavior ON executions_visibility (namespace_id, TemporalWorkflowVersioningBehavior,  (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_temporal_worker_deployment    ON executions_visibility (namespace_id, TemporalWorkerDeployment,  (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_bool_01     ON executions_visibility (namespace_id, Bool01,     (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_bool_02     ON executions_visibility (namespace_id, Bool02,     (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_bool_03     ON executions_visibility (namespace_id, Bool03,     (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_datetime_01 ON executions_visibility (namespace_id, Datetime01, (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_datetime_02 ON executions_visibility (namespace_id, Datetime02, (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_datetime_03 ON executions_visibility (namespace_id, Datetime03, (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_double_01   ON executions_visibility (namespace_id, Double01,   (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_double_02   ON executions_visibility (namespace_id, Double02,   (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_double_03   ON executions_visibility (namespace_id, Double03,   (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_int_01      ON executions_visibility (namespace_id, Int01,      (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_int_02      ON executions_visibility (namespace_id, Int02,      (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_int_03      ON executions_visibility (namespace_id, Int03,      (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_keyword_01  ON executions_visibility (namespace_id, Keyword01,  (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_keyword_02  ON executions_visibility (namespace_id, Keyword02,  (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_keyword_03  ON executions_visibility (namespace_id, Keyword03,  (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_keyword_04  ON executions_visibility (namespace_id, Keyword04,  (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_keyword_05  ON executions_visibility (namespace_id, Keyword05,  (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_keyword_06  ON executions_visibility (namespace_id, Keyword06,  (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_keyword_07  ON executions_visibility (namespace_id, Keyword07,  (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_keyword_08  ON executions_visibility (namespace_id, Keyword08,  (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_keyword_09  ON executions_visibility (namespace_id, Keyword09,  (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE INDEX by_keyword_10  ON executions_visibility (namespace_id, Keyword10,  (COALESCE(close_time, '9999-12-31 23:59:59+00:00')) DESC, start_time DESC, run_id);
CREATE VIRTUAL TABLE executions_visibility_fts_text USING fts5 (
  Text01,
  Text02,
  Text03,
  content='executions_visibility',
  tokenize="unicode61 remove_diacritics 2"
)
/* executions_visibility_fts_text(Text01,Text02,Text03) */;
CREATE TABLE IF NOT EXISTS 'executions_visibility_fts_text_data'(id INTEGER PRIMARY KEY, block BLOB);
CREATE TABLE IF NOT EXISTS 'executions_visibility_fts_text_idx'(segid, term, pgno, PRIMARY KEY(segid, term)) WITHOUT ROWID;
CREATE TABLE IF NOT EXISTS 'executions_visibility_fts_text_docsize'(id INTEGER PRIMARY KEY, sz BLOB);
CREATE TABLE IF NOT EXISTS 'executions_visibility_fts_text_config'(k PRIMARY KEY, v) WITHOUT ROWID;
CREATE VIRTUAL TABLE executions_visibility_fts_keyword_list USING fts5 (
  TemporalChangeVersion,
  BinaryChecksums,
  BuildIds,
  TemporalPauseInfo,
  KeywordList01,
  KeywordList02,
  KeywordList03,
  content='executions_visibility',
  tokenize="unicode61 remove_diacritics 0 categories 'C* L* M* N* P* S* Z*' separators '♡'"
)
/* executions_visibility_fts_keyword_list(TemporalChangeVersion,BinaryChecksums,BuildIds,TemporalPauseInfo,KeywordList01,KeywordList02,KeywordList03) */;
CREATE TABLE IF NOT EXISTS 'executions_visibility_fts_keyword_list_data'(id INTEGER PRIMARY KEY, block BLOB);
CREATE TABLE IF NOT EXISTS 'executions_visibility_fts_keyword_list_idx'(segid, term, pgno, PRIMARY KEY(segid, term)) WITHOUT ROWID;
CREATE TABLE IF NOT EXISTS 'executions_visibility_fts_keyword_list_docsize'(id INTEGER PRIMARY KEY, sz BLOB);
CREATE TABLE IF NOT EXISTS 'executions_visibility_fts_keyword_list_config'(k PRIMARY KEY, v) WITHOUT ROWID;
CREATE TRIGGER executions_visibility_ai AFTER INSERT ON executions_visibility
BEGIN

  INSERT INTO executions_visibility_fts_text (
    rowid,
    Text01,
    Text02,
    Text03
  ) VALUES (
    NEW.rowid,
    NEW.Text01,
    NEW.Text02,
    NEW.Text03
  );

  INSERT INTO executions_visibility_fts_keyword_list (
    rowid,
    TemporalChangeVersion,
    BinaryChecksums,
    BuildIds,
    TemporalPauseInfo,
    KeywordList01,
    KeywordList02,
    KeywordList03
  ) VALUES (
    NEW.rowid,
    NEW.TemporalChangeVersion,
    NEW.BinaryChecksums,
    NEW.BuildIds,
    NEW.TemporalPauseInfo,
    NEW.KeywordList01,
    NEW.KeywordList02,
    NEW.KeywordList03
  );
END;
CREATE TRIGGER executions_visibility_ad AFTER DELETE ON executions_visibility
BEGIN

  INSERT INTO executions_visibility_fts_text (
    executions_visibility_fts_text,
    rowid,
    Text01,
    Text02,
    Text03
  ) VALUES (
    'delete',
    OLD.rowid,
    OLD.Text01,
    OLD.Text02,
    OLD.Text03
  );

  INSERT INTO executions_visibility_fts_keyword_list (
    executions_visibility_fts_keyword_list,
    rowid,
    TemporalChangeVersion,
    BinaryChecksums,
    BuildIds,
    TemporalPauseInfo,
    KeywordList01,
    KeywordList02,
    KeywordList03
  ) VALUES (
    'delete',
    OLD.rowid,
    OLD.TemporalChangeVersion,
    OLD.BinaryChecksums,
    OLD.BuildIds,
    OLD.TemporalPauseInfo,
    OLD.KeywordList01,
    OLD.KeywordList02,
    OLD.KeywordList03
  );
END;
CREATE TRIGGER executions_visibility_au AFTER UPDATE ON executions_visibility
BEGIN

  INSERT INTO executions_visibility_fts_text (
    executions_visibility_fts_text,
    rowid,
    Text01,
    Text02,
    Text03
  ) VALUES (
    'delete',
    OLD.rowid,
    OLD.Text01,
    OLD.Text02,
    OLD.Text03
  );
  INSERT INTO executions_visibility_fts_text (
    rowid,
    Text01,
    Text02,
    Text03
  ) VALUES (
    NEW.rowid,
    NEW.Text01,
    NEW.Text02,
    NEW.Text03
  );

  INSERT INTO executions_visibility_fts_keyword_list (
    executions_visibility_fts_keyword_list,
    rowid,
    TemporalChangeVersion,
    BinaryChecksums,
    BuildIds,
    TemporalPauseInfo,
    KeywordList01,
    KeywordList02,
    KeywordList03
  ) VALUES (
    'delete',
    OLD.rowid,
    OLD.TemporalChangeVersion,
    OLD.BinaryChecksums,
    OLD.BuildIds,
    OLD.TemporalPauseInfo,
    OLD.KeywordList01,
    OLD.KeywordList02,
    OLD.KeywordList03
  );
  INSERT INTO executions_visibility_fts_keyword_list (
    rowid,
    TemporalChangeVersion,
    BinaryChecksums,
    BuildIds,
    TemporalPauseInfo,
    KeywordList01,
    KeywordList02,
    KeywordList03
  ) VALUES (
    NEW.rowid,
    NEW.TemporalChangeVersion,
    NEW.BinaryChecksums,
    NEW.BuildIds,
    NEW.TemporalPauseInfo,
    NEW.KeywordList01,
    NEW.KeywordList02,
    NEW.KeywordList03
  );
END;
