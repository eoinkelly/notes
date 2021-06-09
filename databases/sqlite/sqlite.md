# SQLite

## Versions installed

```
// Add these lines to a project to see which FTS (if any) you have
NSLog(@"Has FTS5: %d", sqlite3_compileoption_used("SQLITE_ENABLE_FTS5"));
NSLog(@"Has FTS4: %d", sqlite3_compileoption_used("SQLITE_ENABLE_FTS4"));
NSLog(@"Has FTS3: %d", sqlite3_compileoption_used("SQLITE_ENABLE_FTS3"));
```

* From my tests iOS 10.3 has FTS3 only
* Apparently iOS 11 will have FTS5 - http://www.openradar.me/29850081

On my mac I have two versions of the `sqlite` command installed:

```
# default macOS
/usr/bin/sqlite3 --version
-- Loading resources from /Users/eoinkelly/.sqliterc
3.16.0 2016-11-04 19:09:39 0e5ffd9123d6d2d2b8f3701e8a73cc98a3a7ff5f


# homebrew
/usr/local/opt/sqlite/bin/sqlite3 --version
-- Loading resources from /Users/eoinkelly/.sqliterc
3.19.3 2017-06-08 14:26:16 0ee482a1e0eae22e08edc8978c9733a96603d4509645f348ebf55b579e89636b
```

The mac text stub .tbd files seem to reference this built-in dylib

    /usr/lib/libsqlite3.dylib

## Extensions

## FTS5 Full text search

* Included in sqlite.c (the "amalgamation") since 3.9.0 but is **disabled by default**
    * need to set `SQLITE_ENABLE_FTS5` before compilation to enable it
* Can also be built as a loadable extension (static lib) and then statically linked into your app
* Apparently it can also sort results by relevance

It seems that FTS4 and FTS5 are mutually exclusive

## SQLCipher

https://www.zetetic.net/sqlcipher/

> SQLCipher is an open source extension to SQLite that provides transparent
> 256-bit AES encryption of database files

* has paid and community editions
* builds a static lib (`libsqlcipher.a`)
* SQLiteManager is a SQLite GUI which can handle encrypting/decrypting databases encrypted by SQLCipher


