Common useful C libraries

- libcURL
- libGlib
- libGSL
- libSQLite3
- libXML2

```
# ubuntu 18 package names

libcurl4-gnutls-dev - development files and documentation for libcurl (GnuTLS flavour)
libcurl4-nss-dev - development files and documentation for libcurl (NSS flavour)
libcurl4-openssl-dev - development files and documentation for libcurl (OpenSSL flavour)

libglib2.0-0 - GLib library of C routines
libglib2.0-bin - Programs for the GLib library
libglib2.0-data - Common files for GLib library
libglib2.0-dev - Development files for the GLib library
libglib2.0-dev-bin - Development utilities for the GLib library
libglib2.0-doc - Documentation files for the GLib library

libgsl-dbg - GNU Scientific Library (GSL) -- debug symbols package
libgsl-dev - GNU Scientific Library (GSL) -- development package
libgsl23 - GNU Scientific Library (GSL) -- library package
libgslcblas0 - GNU Scientific Library (GSL) -- blas library package

libsqlite3-dev - SQLite 3 development files

libxml2-dev - Development files for the GNOME XML library
```

To use a library, you have to tell the compiler that you will be importing
functions from the library twice:

1. once for the compilation
1. once for the linker.

Both these things happen at _compile time_.

For a library in a standard location, the two declarations happen via:

1. an #include in the text of the program
1. a l flag on the compiler line.
