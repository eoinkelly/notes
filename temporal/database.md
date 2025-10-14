
Introspecting the sqlite version of the Temporal DB

- 45 tables
- 3 triggers

A very common set of identifiers for a row is

1. shard ID
2. namespace ID
3. workflow ID
4. run ID

Makes heavy use of [Proto3](https://protobuf.dev/programming-guides/proto3/) protobuf encoding. A lot of tables have the tuple `(data, data_encoding)` where `data_encoding` is always "Proto3".
=> not super easy to introspect the contents of some tables

There are 2 namespaces by default:

1. `default`
2. `temporal-system`

so temporal uses temporal tasks to manage stuff? does it run on workers too? or does the temporal server include a worker?

the history of the server is stored as a tree.
one table of node details and another table of the "tree" that holds the relationship between the nodes
I guess this is because workflows can spawn workflows?

A bunch of tables seem to exist to allow searching of the workflows via TemporalUI and tctl - the `executions_visibility` table seems to be the main one that corresponds to the Temporal UI view

It uses the FTS5 extension which presumably manages a bunch of those tables under the hood to implement the fts
This also explains the 3 triggers

```sql
-- from schema.sql
CREATE VIRTUAL TABLE executions_visibility_fts_text USING fts5 (
  Text01,
  Text02,
  Text03,
  content='executions_visibility',
  tokenize="unicode61 remove_diacritics 2"
)
```

## Tables

- activity_info_maps
- buffered_events
- build_id_to_task_queue
- chasm_node_maps
- child_execution_info_maps
- cluster_membership
- cluster_metadata_info

- current_executions
- executions
- executions_visibility
- executions_visibility_fts_keyword_list
- executions_visibility_fts_keyword_list_config
- executions_visibility_fts_keyword_list_data
- executions_visibility_fts_keyword_list_docsize
- executions_visibility_fts_keyword_list_idx
- executions_visibility_fts_text
- executions_visibility_fts_text_config
- executions_visibility_fts_text_data
- executions_visibility_fts_text_docsize
- executions_visibility_fts_text_idx

- history_immediate_tasks
- history_node
- history_scheduled_tasks
- history_tree

- namespace_metadata
- namespaces

- nexus_endpoints
- nexus_endpoints_partition_status

- queue
- queue_messages
- queue_metadata
- queues

- replication_tasks
- replication_tasks_dlq

- request_cancel_info_maps
- shards

- signal_info_maps
- signals_requested_sets

- task_queue_user_data
- task_queues
- tasks

- timer_info_maps
- timer_tasks

- transfer_tasks
- visibility_tasks