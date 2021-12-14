# Cloudflare serverless offerings

## Workers

* have a default route under .workers.dev domain
* you can add custom routes
* can run on a schedule
* they do have an online editor like lambda
* you can use serverless framework to create them

## KV store

* key value storage
* organised into "namespaces", which have a name and an ID (seems to be akin to a "database" in redis)
    * you can bind namespace to a worker
* The entire request size must be less than 100 megabytes

> KV achieves this performance by being eventually-consistent. Changes are
> immediately visible in the edge location at which they're made, but may take up
> to 60 seconds to propagate to all other edge locations. In particular,
> propagation of changes takes longer to locations which have recently read a
> previous version of a given key (including reads that indicated the key didn't
> exist). Workers KV isn’t ideal for situations where you need support for atomic
> operations or where values must be read and written in a single transaction.
>
> All values are encrypted at rest with 256-bit AES-GCM, and only decrypted by
> the process executing your Worker scripts or responding to your API requests.

## Durable objects

* still in beta as of Nov 2021
* no final pricing yet

> Manage state with low-latency coordination and consistent real-time storage
> for your Workers. Data is intelligently distributed across the Cloudflare
> network and stored closest to where it is accessed.

