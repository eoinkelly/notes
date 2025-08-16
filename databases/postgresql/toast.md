# TOAST (The Oversized-Attribute Storage Technique).

http://www.postgresql.org/docs/current/interactive/storage-toast.html

- PostgreSQL uses a fixed page size (commonly 8 kB), and does not allow tuples
  to span multiple pages.
- Therefore, it is not possible to store very large field values directly. To
  overcome this limitation, large field values are compressed and/or broken up
  into multiple physical rows.
- This happens transparently to the user, with only small impact on most of the
  backend code. The technique is affectionately known as TOAST

- TOAST imposes some overhead
- only data types which can generate values larger than 8kB need to use TOAST
- a data type must have a `varlena` representation to use TOAST
