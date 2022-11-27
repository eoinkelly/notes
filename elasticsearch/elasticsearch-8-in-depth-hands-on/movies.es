// Movies example

// Create the movies index
PUT /movies/
{
  "settings": {
    "analysis": {
      "filter": {
        "eoin_mild_trunc": {
          "type": "truncate",
          "length": 40
        }
      },
      "analyzer": {
        "eoin1": {
          "type": "standard", // <-- configure an existing analyzer
          "stopwords": "_english_"
        },
        "eoin2": {
          "type": "custom", // <-- custom => create new analyzer not config an existing
          "char_filter": [
            "html_strip"
          ],
          "tokenizer": "uax_url_email",
          "filter": [
            "eoin_mild_trunc",
            "lowercase",
            "asciifolding"
          ]
        }
      }
    }
  },
  "mappings": {
    "properties": {
      "id": {
        "type": "integer"
      },
      "year": {
        "type": "date"
      },
      "title": {
        "type": "text",
        "analyzer": "english",
        "fields": {
          "keyword": {
            "type": "keyword"
          }
        }
      },
      "genre": {
        "type": "keyword"
      }
    }
  }
}

GET /movies/_settings
GET /movies/_mapping
GET /movies/_search
GET /movies/_count

DELETE /movies


// Test a custom analyzer
POST movies/_analyze
{
  "analyzer": "eoin2",
  "text": "This <b>IS</b> déjà  a sEnTenCE 1 4 all! !!@ eoin@foo.com http://www.example.com/blah/blah/blah.jpg"
}
  // "text": "Is this <b>déjà vu</b>?"
  // "text": "This IS a sEnTenCE"

// Bulk load data
// curl -H "Content-Type: application/json" -XPUT localhost:9200/_bulk --data-binary @movies.json

// Create a movie manually
PUT /movies/_doc/111
{
  "title": "Eoin 1",
  "year": 2022,
  "genre": [
    "aaa",
    "bbb"
  ]
}

GET /movies/_doc/111

// This replaces the whole document
POST /movies/_doc/111
{
  "title": "Eoin 2"
}

// This updates just the title field leaving the other fields as is (note the "doc")
POST /movies/_update/111?if_seq_no=13&if_primary_term=1
{
  "doc": {
    "title": "Eoin 2"
  }
}

// This uses the (seq_no, primary_term) tuple to only write to the revision of the doc that we saw
POST /movies/_update/111?if_seq_no=13&if_primary_term=1
{
  "doc": {
    "title": "Eoin 2"
  }
}

POST /movies/_update/111?retry_on_conflict=3
{
  "doc": {
    "title": "Eoin 2"
  }
}

DELETE /movies/_doc/111

GET /movies/_search
{
  "query": {
    "match_phrase": {
      "title": {
        "query": "beyond trek star",
        // slop: 2 will find "star trek" for "trek star"
        // slop: 3 will find "star trek beyond" from "trek beyond star"
        // slop: 4 will find "star trek beyond" from "beyond trek star"
        "slop": 4
      }
    }
  }
}

GET /movies/_search
{
  "query": {
    "term": {
      "title.keyword": "Star Trek Beyond"
    }
  }
}