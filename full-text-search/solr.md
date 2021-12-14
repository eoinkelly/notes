
# Solr

## Installing
```
brew install solr

brew services start solr
# or
solr start

❯ 'curl' -v http://localhost:8983/solr/
```

/usr/local/Cellar/solr/8.4.1_2

Default config

    -Dsolr.log.dir = /usr/local/var/log/solr
    -Dsolr.log.muteconsole-Dsolr.solr.home=/usr/local/var/lib/solr


Warnings I get on macOS

    ❯ solr start
    *** [WARN] *** Your open file limit is currently 256.
    It should be set to 65000 to avoid operational disruption.
    If you no longer wish to see this warning, set SOLR_ULIMIT_CHECKS to false in your profile or solr.in.sh
    *** [WARN] ***  Your Max Processes Limit is currently 2784.
    It should be set to 65000 to avoid operational disruption.
    If you no longer wish to see this warning, set SOLR_ULIMIT_CHECKS to false in your profile or solr.in.sh


Create a core

    ❯ solr create_core -c eoin_1 -d _default
    WARNING: Using _default configset with data driven schema functionality. NOT RECOMMENDED for production use.
            To turn off: bin/solr config -c eoin_1 -p 8983 -action set-user-property -property update.autoCreateFields -value false

    Created new core 'eoin_1'


## Overview

* based on Lucene
    * many of its performance characteristics come from lucene e.g.
        * near real-time
* optimised for text centric data
    * doesn't have the aggregation stuff that Elasticsearch does
* handles "unstructured" data
    * unstructured from the POV of a computer, humans see structure in it
    * computer sees: a stream of characters
    * human sees: characters, words, sentences, meaning
    * the document has structure that humans can see but computers cannot
* in a regular SQL DB we add structure in a way that the computer can see it
    * each bit of data is broken out into it's own column which has has a static type
* documents
    * must be flat.
    * a field can contain multiple values (e.g. array like) but cannot contain nested fields
        * this is different to Elasticsearch
