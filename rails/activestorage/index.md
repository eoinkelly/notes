# Active storage

```ruby
# Example ActiveRecord class with an attachment
class PageVersionImage < ApplicationRecord
  has_one_attached :file
  validates :file,
    content_type: { in: ["image/png", "image/jpg", "image/jpeg", "image/webp"], message: "incorrect type" },
    size: { less_than: 6.megabytes, message: "exceed maximum size" }
end
```

## How the tables are used

1. Rails knows from the ruby code in the class telling it that it should find
   and ActiveStorage attachment called `file`
1. It starts by finding the corresponding blob ID in the attachments table
    ```sql
    select blob_id from active_storage_attachments where name = 'file' and record_type = 'PageVersionImage' and record_id = '0966589d-a80b-432f-a617-8201933565ad';
    --                blob_id
    -- --------------------------------------
    --  c2a1b0c7-c42a-464f-b853-fc1141ebf13d
    -- (1 row)
    ```
1. Then using the blob_id to find the blob:
    ```sql
    select * from active_storage_blobs where id = 'c2a1b0c7-c42a-464f-b853-fc1141ebf13d';
    --                   id                  |             key              |                 filename                 | content_type |                           metadata                           | byte_size |         checksum         |         created_at         | service_name
    -- --------------------------------------+------------------------------+------------------------------------------+--------------+--------------------------------------------------------------+-----------+--------------------------+----------------------------+--------------
    --  c2a1b0c7-c42a-464f-b853-fc1141ebf13d | 7n2wq10jop4macqv2hm68nz1hsls | 98f485e4-d279-4ae2-a9a8-5fe1d2e97cac.jpg | image/webp   | {"identified":true,"width":900,"height":603,"analyzed":true} |     42418 | YRS3zYTeOUG5KtpOEpAWMg== | 2023-04-05 18:54:33.303262 | local
    -- (1 row)
    ```
1. Use the blob to gererate the URL to the actual file
    - Url will depend on the service
    - ?? How are key, filename and checksum used in generating the URL?
    - Checksum is an MD5 checksum. Rails computes it if you use proxy upload. JS
      computes it if you use direct upload
    - `key` field is used to find the file on the service
    - An example using `:local` service:
        - In the `:local` service a key of `7n2wq10jop4macqv2hm68nz1hsls` is
          stored in a file on disk at
          `<local-storage-root>/7n/2w/7n2wq10jop4macqv2hm68nz1hsls`
        - This file is the image file:
            ```
            $ file 7n2wq10jop4macqv2hm68nz1hsls
            7n2wq10jop4macqv2hm68nz1hsls: RIFF (little-endian) data, Web/P image
            ```

## other

Variants are not actually created until something tries to download the file

```ruby
[1] pry(main)> img = PageVersionImage.order(created_at: :desc).first

[2] pry(main)> img.file.blob.variable?
  ActiveStorage::Attachment Load (6.0ms)  SELECT "active_storage_attachments".* FROM "active_storage_attachments" WHERE "active_storage_attachments"."record_id" = $1 AND "active_storage_attachments"."record_type" = $2 AND "active_storage_attachments"."name" = $3 LIMIT $4  [["record_id", "0966589d-a80b-432f-a617-8201933565ad"], ["record_type", "PageVersionImage"], ["name", "file"], ["LIMIT", 1]]
  ActiveStorage::Blob Load (0.7ms)  SELECT "active_storage_blobs".* FROM "active_storage_blobs" WHERE "active_storage_blobs"."id" = $1 LIMIT $2  [["id", "c2a1b0c7-c42a-464f-b853-fc1141ebf13d"], ["LIMIT", 1]]
=> true

[8] pry(main)> app.rails_blob_path  img.file.variant(resize_to_limit: [100, 100])
=> "/rails/active_storage/representations/proxy/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaEpJaWxqTW1FeFlqQmpOeTFqTkRKaExUUTJOR1l0WWpnMU15MW1ZekV4TkRGbFltWXhNMlFHT2daRlZBPT0iLCJleHAiOm51bGwsInB1ciI6ImJsb2JfaWQifX0=--92c23c6c5ed3610cdae22bf3da3064a826e1a0b7/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaDdCem9MWm05eWJXRjBTU0lKZDJWaWNBWTZCa1ZVT2hSeVpYTnBlbVZmZEc5ZmJHbHRhWFJiQjJscGFXaz0iLCJleHAiOm51bGwsInB1ciI6InZhcmlhdGlvbiJ9fQ==--148ec7b8ece10cfd166cfa976dd056cdfd1b23f5/98f485e4-d279-4ae2-a9a8-5fe1d2e97cac.jpg"

# note the file is not actually created until it is first downloaded
```

## Database tables

### active_storage_attachments

- https://edgeapi.rubyonrails.org/classes/ActiveStorage/Attachment.html
-

```
=# \d active_storage_attachments
                          Table "public.active_storage_attachments"
   Column    |            Type             | Collation | Nullable |         Default
-------------+-----------------------------+-----------+----------+--------------------------
 id          | uuid                        |           | not null | public.gen_random_uuid()
 name        | character varying           |           | not null |
 record_type | character varying           |           | not null |
 record_id   | uuid                        |           | not null |
 blob_id     | uuid                        |           | not null |
 created_at  | timestamp without time zone |           | not null |
Indexes:
    "active_storage_attachments_pkey" PRIMARY KEY, btree (id)
    "index_active_storage_attachments_on_blob_id" btree (blob_id)
    "index_active_storage_attachments_uniqueness" UNIQUE, btree (record_type, record_id, name, blob_id)
Foreign-key constraints:
    "fk_rails_c3b3935057" FOREIGN KEY (blob_id) REFERENCES active_storage_blobs(id)

=# select * from active_storage_attachments limit 3;
                  id                  | name |   record_type    |              record_id               |               blob_id                |         created_at
--------------------------------------+------+------------------+--------------------------------------+--------------------------------------+----------------------------
 4da88a57-e278-4efd-8ab7-0d4b3dbb0ee6 | file | PageVersionImage | 55a05305-ad9b-46ad-9f2c-08589d384bb4 | d9ff4896-fcdc-40b3-8dea-93ed2cd07ffd | 2020-05-13 02:24:32.32019
 07cd66db-b96f-4c91-9ebd-1ad7120d1e4e | file | PageVersionImage | e432de1d-898e-4a53-965d-c33ace342804 | 4c355154-923f-41a3-a936-67f5e150b4b5 | 2020-05-13 02:28:00.204267
 8aa6b6b1-2417-4c9f-8e30-cd2332625d05 | file | PageVersionImage | 1878d1c3-0b96-49c9-8e7f-64786fe13afb | 18d1e15e-3720-40d8-93aa-1b3a2aa38157 | 2020-05-13 03:19:03.527785
(3 rows)
```

### active_storage_blobs

> A blob is a record that contains the metadata about a file and a key for where
> that file resides on the service.

```
=# \d active_storage_blobs
                             Table "public.active_storage_blobs"
    Column    |            Type             | Collation | Nullable |         Default
--------------+-----------------------------+-----------+----------+--------------------------
 id           | uuid                        |           | not null | public.gen_random_uuid()
 key          | character varying           |           | not null |
 filename     | character varying           |           | not null |
 content_type | character varying           |           |          |
 metadata     | text                        |           |          |
 byte_size    | bigint                      |           | not null |
 checksum     | character varying           |           |          |
 created_at   | timestamp without time zone |           | not null |
 service_name | character varying           |           |          |
Indexes:
    "active_storage_blobs_pkey" PRIMARY KEY, btree (id)
    "index_active_storage_blobs_on_key" UNIQUE, btree (key)
Referenced by:
    TABLE "active_storage_variant_records" CONSTRAINT "fk_rails_993965df05" FOREIGN KEY (blob_id) REFERENCES active_storage_blobs(id)
    TABLE "active_storage_attachments" CONSTRAINT "fk_rails_c3b3935057" FOREIGN KEY (blob_id) REFERENCES active_storage_blobs(id)

=# select * from active_storage_blobs limit 3;
                  id                  |             key              |                 filename                 | content_type |                           metadata                           | byte_size |         checksum         |         created_at         | service_name
--------------------------------------+------------------------------+------------------------------------------+--------------+--------------------------------------------------------------+-----------+--------------------------+----------------------------+--------------
 076ca6a4-e86a-417a-8a65-24af355e1741 | m2kmod6ymrm06apmytkwaw04xp1a | 8eaa47b2-2217-451b-ae84-feea60917bc0.jpg | image/jpeg   | {"identified":true,"width":900,"height":540,"analyzed":true} |     97147 | FJS6oQrntCFDbNYyvjkXtQ== | 2022-09-19 04:33:04.048521 | amazon
 ac23a504-194b-4f40-abb1-229e376791f9 | rv8f3ta3qkuop8y4f62yidctlmvu | eb909047-8752-4b59-b6e6-116763cd7b94.jpg | image/jpeg   | {"identified":true,"width":686,"height":460,"analyzed":true} |     64388 | 2DgSP+RX3GC/1MDLB8fPuQ== | 2022-09-19 04:34:19.432268 | amazon
 6d0c66b6-fd3f-4301-a975-1f5bf7a2eb45 | 6d48zni37j27c7qgh5dubk8c6kx7 | 575ed65f-a7ca-488f-aed4-a59235e2ebc9.jpg | image/jpeg   | {"identified":true,"width":230,"height":207,"analyzed":true} |     18311 | m9PQMVJtPP/yh95+6RhJCA== | 2022-09-19 04:34:23.708211 | amazon
```

### active_storage_variant_records

- Stores the variant digests for a blob ( a blob can have many variants and
  therefore variant digests)
- https://github.com/rails/rails/blob/main/activestorage/app/models/active_storage/variation.rb
- I think the variation digest is a checksum of the list of transforms for that
  variant. It lets rails see if the variant has already been created

An ActiveStorage::Variation is a set of transformations that can be applied to a
blob to create a variant.

```
=# \d active_storage_variant_records
                     Table "public.active_storage_variant_records"
      Column      |       Type        | Collation | Nullable |         Default
------------------+-------------------+-----------+----------+--------------------------
 id               | uuid              |           | not null | public.gen_random_uuid()
 blob_id          | uuid              |           | not null |
 variation_digest | character varying |           | not null |
Indexes:
    "active_storage_variant_records_pkey" PRIMARY KEY, btree (id)
    "index_active_storage_variant_records_uniqueness" UNIQUE, btree (blob_id, variation_digest)
Foreign-key constraints:
    "fk_rails_993965df05" FOREIGN KEY (blob_id) REFERENCES active_storage_blobs(id)

=# select * from active_storage_variant_records limit 3;
                  id                  |               blob_id                |       variation_digest
--------------------------------------+--------------------------------------+------------------------------
 6f40cc3d-f556-41d2-805e-ac88d75f5bc7 | 27069415-975a-47b8-8424-a6e9dda0dffc | fO2apR6l7KPpY/1qOxm8klDE0nM=
 5356cdad-ebd2-4283-8dd0-62f19cf05b62 | c533357c-7f2f-4205-abc0-6b8ae04ff59e | 3Ju4hz3U3oZIVvmQJtr3kq71GeM=
 fb5a2b59-9c19-4291-bad2-0dc56496b631 | 24b617a5-4666-49d4-9fa5-11099462d8c1 | NniMYxK0H0XOpC2EhaVx2MKmgfg=
```

## Misc code examples

```ruby
# set app default to proxy
Rails.application.config.active_storage.resolve_model_to_route = :rails_storage_proxy

# both these change by whether the app default is to proxy or redirect
url_for(some_image_ob)
rails_blob_(path|url)(some_image_ob, disposition: :inline|:attachment) # same as above but lets you set disposition

# explicitly ask for the proxy url
rails_storage_proxy_path(@user.avatar)
rails_storage_proxy_url(@user.avatar)

# from a model or other ruby class (notice the only_path)[
Rails.application.routes.url_helpers.rails_blob_path(user.avatar, only_path: true)


# disable drawing of all ActiveStorage routes
config.active_storage.draw_routes = false


# this will generate a req to the AS redirect controller which will then rediret to the actual file on storage
file.representation(resize_to_limit: [100, 100]).url

# add .processed to generate the URL for the image immediately
file.representation(resize_to_limit: [100, 100]).url
```

the app uses url_for() url_for(img.variant())

in tests: url_for(main_image.variant(resize_to_limit: [336,
225])).remove('http://www.example.com')
