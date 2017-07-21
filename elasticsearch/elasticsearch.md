# Elasticsearch

## Overview

* Kibana
    * http://localhost:5601
    * Web UI for elasticsearch

```
Relational DB  ⇒ Databases ⇒ Tables ⇒ Rows      ⇒ Columns
Elasticsearch  ⇒ Indices   ⇒ Types  ⇒ Documents ⇒ Fields
```

### Installing (macOS)

```
brew install elasticsearch kibana logstash
kibana-plugin install x-pack
```

## Examples

```bash
curl -XPOST "http://localhost:9200/_shutdown"
# does not seem to work?


curl -XGET "http://localhost:9200/?pretty"
{
  "name": "FNAhp-l",
  "cluster_name": "elasticsearch_eoinkelly",
  "cluster_uuid": "5E5Aa-tVQxeMkb9SUQAtqA",
  "version": {
    "number": "5.5.0",
    "build_hash": "260387d",
    "build_date": "2017-06-30T23:16:05.735Z",
    "build_snapshot": false,
    "lucene_version": "6.6.0"
  },
  "tagline": "You Know, for Search"
}


curl -XGET "http://localhost:9200/_cluster/health"
{
  "cluster_name": "elasticsearch_eoinkelly",
  "status": "yellow",
  "timed_out": false,
  "number_of_nodes": 1,
  "number_of_data_nodes": 1,
  "active_primary_shards": 30,
  "active_shards": 30,
  "relocating_shards": 0,
  "initializing_shards": 0,
  "unassigned_shards": 30,
  "delayed_unassigned_shards": 0,
  "number_of_pending_tasks": 0,
  "number_of_in_flight_fetch": 0,
  "task_max_waiting_in_queue_millis": 0,
  "active_shards_percent_as_number": 50
}
```
