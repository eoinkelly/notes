PUT /shakespeare
{
  "mappings": {
    "properties": {
      "speaker": {
        "type": "keyword"
      },
      "play_name": {
        "type": "keyword"
      },
      "line_id": {
        "type": "integer"
      },
      "speech_number": {
        "type": "integer"
      }
    }
  }
}

// Load data
//  curl -XPUT -H "Content-Type: application/json" http://localhost:9200/shakespeare/_bulk --data-binary @shakespeare_8.0.json

// Check current state
GET /shakespeare/_search