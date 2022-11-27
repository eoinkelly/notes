# Lucene

Sources

* https://lucene.apache.org/core/9_4_2/core/org/apache/lucene/codecs/lucene94/package-summary.html

Overview

* Terminology
  * term = a tuple `(field_name, sequence_of_bytes)`
  * field = **named** sequence of terms
  * document = sequence of fields
  * index = sequence of documents
* Lucene stores:
  1. terms
  2. statistics about the terms e.g. which documents include the term, where in the doc the term appears
* Terms are stored with the field name so the same word appearing in different fields is not considered the same term e.g. if both `title` and `body` contain the word `hello`, Lucene will store separate `hello` terms for each field
* Lucene has two ways of saving a field:
  1. A _Stored_ field
    * the text is stored literally (not inverted)
  2. An _Indexed_ field
    * The data in the field is stored inverted
    * the raw field value is tokenized and stored as terms.
    * sometimes tokenization does nothing and emits just the raw value e.g. you would do this for an ID field
* The same field can (and often is) stored as both _stored_ and _indexed_.
* A lucene index:
  * a directory of files
  * the format of the files often changes between Lucene versions
  * may be composed of multiple sub-indexes, or **segments**.
  * Each segment is a fully independent index, which could be searched separately.
  * Lucene evolves the index over time as data is added and removed.
    * Additions will eventually require adding new segments
    * removals create gaps which are revolved by merging existing segments
  * There are multiple Segments in an Index
  * A search can happen across multiple Indexes
  * In dockerized ES the data is stored in `/usr/share/elasticsearch/data/nodes/0/indices/<uuid_of_index>`
  * You can force merge segments
* Document number
  * Lucene refers to documents by an **integer** document number (starting at 0 and incrementing by 1)
  * Segments are allocated a range of document numbers
  * The document number is unique within the Segement (not within the index)
  * The segment has a base number which is added to the document number to create an external number
  * A document's number can change over time e.g. when segements are merged
* Deleted documents are not dropped until segments are merged!
* All files which belong to a Segment have the same name (w. different extensions)
* The files stored on disk are serializations of the data structures (implemented as Java classes) that Lucene uses

```bash
$ ls -lh /usr/share/elasticsearch/data/nodes/0/indices/sTDG86UlQB6JWj2NrmaX5Q/0/index
total 26M

# _a segment files
-rw-rw-r-- 1 elasticsearch root   479 Nov 22 00:52 _a.cfe # .cfe and .cfs together are a compound files i.e. container for other index files (used on systems which run out of file handles)
-rw-rw-r-- 1 elasticsearch root  2.0M Nov 22 00:52 _a.cfs #  see ^^^^
-rw-rw-r-- 1 elasticsearch root   369 Nov 22 00:52 _a.si # segment info

# _b segment files
-rw-rw-r-- 1 elasticsearch root   157 Nov 22 00:52 _b.fdm
-rw-rw-r-- 1 elasticsearch root  2.8M Nov 22 00:52 _b.fdt # stored fields for documents
-rw-rw-r-- 1 elasticsearch root   116 Nov 22 00:52 _b.fdx # pointers to field data
-rw-rw-r-- 1 elasticsearch root   16K Nov 22 00:52 _b.fnm # field info
-rw-rw-r-- 1 elasticsearch root  141K Nov 22 00:52 _b.kdd
-rw-rw-r-- 1 elasticsearch root   480 Nov 22 00:52 _b.kdi
-rw-rw-r-- 1 elasticsearch root  1.2K Nov 22 00:52 _b.kdm
-rw-rw-r-- 1 elasticsearch root  129K Nov 22 00:52 _b.nvd
-rw-rw-r-- 1 elasticsearch root  1.6K Nov 22 00:52 _b.nvm
-rw-rw-r-- 1 elasticsearch root   582 Nov 22 00:52 _b.si

-rw-rw-r-- 1 elasticsearch root  2.9M Nov 22 00:52 _b_Lucene80_0.dvd
-rw-rw-r-- 1 elasticsearch root   14K Nov 22 00:52 _b_Lucene80_0.dvm
-rw-rw-r-- 1 elasticsearch root  776K Nov 22 00:52 _b_Lucene84_0.doc
-rw-rw-r-- 1 elasticsearch root 1015K Nov 22 00:52 _b_Lucene84_0.pos # stores positions where a term occurs in index
-rw-rw-r-- 1 elasticsearch root  5.3M Nov 22 00:52 _b_Lucene84_0.tim # term dictionary
-rw-rw-r-- 1 elasticsearch root   85K Nov 22 00:52 _b_Lucene84_0.tip
-rw-rw-r-- 1 elasticsearch root  9.0K Nov 22 00:52 _b_Lucene84_0.tmd

-rw-rw-r-- 1 elasticsearch root   479 Nov 22 00:52 _c.cfe
-rw-rw-r-- 1 elasticsearch root  2.0M Nov 22 00:52 _c.cfs
-rw-rw-r-- 1 elasticsearch root   369 Nov 22 00:52 _c.si

-rw-rw-r-- 1 elasticsearch root   479 Nov 22 00:52 _d.cfe
-rw-rw-r-- 1 elasticsearch root  1.8M Nov 22 00:52 _d.cfs
-rw-rw-r-- 1 elasticsearch root   369 Nov 22 00:52 _d.si

-rw-rw-r-- 1 elasticsearch root   479 Nov 22 00:52 _e.cfe
-rw-rw-r-- 1 elasticsearch root  1.8M Nov 22 00:52 _e.cfs
-rw-rw-r-- 1 elasticsearch root   369 Nov 22 00:52 _e.si

-rw-rw-r-- 1 elasticsearch root   479 Nov 22 00:52 _f.cfe
-rw-rw-r-- 1 elasticsearch root  5.3M Nov 22 00:52 _f.cfs
-rw-rw-r-- 1 elasticsearch root   369 Nov 22 00:52 _f.si

-rw-rw-r-- 1 elasticsearch root   729 Nov 22 00:57 segments_3
-rw-rw-r-- 1 elasticsearch root     0 Nov 22 00:51 write.lock # lock file, only one thread can modify the index at a time
```