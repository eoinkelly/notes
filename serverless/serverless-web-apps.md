# Serverless for web applications

- [Serverless for web applications](#serverless-for-web-applications)
    - [This is about building web applications](#this-is-about-building-web-applications)
    - [Sources](#sources)
    - [Serverless the idea vs serverless the reality](#serverless-the-idea-vs-serverless-the-reality)
    - [Moving problems around](#moving-problems-around)
    - ["serverless" the ideal](#serverless-the-ideal)
        - [Wikipedia has a go](#wikipedia-has-a-go)
        - [Cloudflare has a go](#cloudflare-has-a-go)
        - [Gomomento has a go](#gomomento-has-a-go)
        - [I have a go](#i-have-a-go)
    - [What's in a web app](#whats-in-a-web-app)
    - [Serverless type 1: Backend as a service](#serverless-type-1-backend-as-a-service)
    - [Serverless type 2: Functions as a service](#serverless-type-2-functions-as-a-service)
        - [FaaS Case Study: BBC](#faas-case-study-bbc)
            - [Pros](#pros)
            - [Cons](#cons)
            - [Unclear](#unclear)
            - [Cost](#cost)
    - [Known problems](#known-problems)
    - [How close to the serverless ideals is a standard webapp in fargate?](#how-close-to-the-serverless-ideals-is-a-standard-webapp-in-fargate)
    - [Deno](#deno)
    - [Serverless databases](#serverless-databases)
        - [Firebase (BaaS)](#firebase-baas)
        - [Supabase (BaaS)](#supabase-baas)
        - [Neon (SQL)](#neon-sql)
        - [DynamoDB](#dynamodb)
        - [Macrometa](#macrometa)
        - [Fauna](#fauna)
        - [CloudFlare KV](#cloudflare-kv)
    - [Edge compute](#edge-compute)
    - [Misc Ramblings](#misc-ramblings)

## This is about building web applications

Serverless clearly has use-cases outside of building web applications but web
apps is what I do and care about so I'm focusing on that use-case here.

## Sources

- https://www.serverless.com/blog/serverless-architecture-code-patterns
- https://martinfowler.com/articles/serverless.html
- https://www.readysetcloud.io/blog/allen.helton/i-dont-know-what-serverless-is-anymore/
- https://www.gomomento.com/blog/fighting-off-fake-serverless-bandits-with-the-true-definition-of-serverless

## Serverless the idea vs serverless the reality

We need to be clear about the difference between:

1. Content that discusses the platonic ideal of what serverless could be
2. Content which discusses what serverless actually is in 2023

There is a lot of content out there about what serverless could or should be.
Often it does not clearly explain how that relates to what serverless currently
is, with all the messy trade-offs that reality brings.

## Moving problems around

Serverless in 2022 solves some problems at the operations layer at the expense
of pushing a lot of that complexity into the application architecture.

You need to be super sure that the ops problems it solves are severe enough to
make pushing all that complexity into your app architecture worth it.

## "serverless" the ideal

### Wikipedia has a go

> developers of serverless applications are not concerned with capacity
> planning, configuration, management, maintenance, fault tolerance, or scaling
> of containers, VMs, or physical servers. Serverless computing does not hold
> resources in volatile memory; computing is rather done in short bursts with
> the results persisted to storage. When an app is not in use, there are no
> computing resources allocated to the app. Pricing is based on the actual
> amount of resources consumed by an application
>
> Small teams of developers are able to run code themselves without the
> dependence upon teams of infrastructure and support engineers; more developers
> are becoming DevOps skilled and distinctions between being a software
> developer or hardware engineer are blurring

> A serverless provider allows users to write and deploy code without the hassle
> of worrying about the underlying infrastructure

### Cloudflare has a go

From https://www.cloudflare.com/learning/serverless/what-is-serverless/

> What are the advantages of serverless computing?
>
> Lower costs - Serverless computing is generally very cost-effective, as
> traditional cloud providers of backend services (server allocation) often
> result in the user paying for unused space or idle CPU time.
>
> Simplified scalability - Developers using serverless architecture don't have
> to worry about policies to scale up their code. The serverless vendor handles
> all of the scaling on demand.
>
> Simplified backend code - With FaaS, developers can create simple functions
> that independently perform a single purpose, like making an API call.
>
> Quicker turnaround - Serverless architecture can significantly cut time to
> market. Instead of needing a complicated deploy process to roll out bug fixes
> and new features, developers can add and modify code on a piecemeal basis.

This sounds like Microservices rather than serverless per-se

### Gomomento has a go

What serverless should be (according to gomomento in
https://www.gomomento.com/blog/fighting-off-fake-serverless-bandits-with-the-true-definition-of-serverless
)

1. Simplicity
1. Instant start
1. Instant elasticity
1. Best practices by default

Faux-serverless bandit tricks ‍

1. Provisioned capacity
1. Autoscaling with no mention of instant
1. Capacity minimums
1. Instances
1. Maintenance windows

From their list of requirements, almost everything called "serverless" in 2023
are not serverless. DynamoDB might be the exception. Even AWS Lambdas allow
provisioned capacity to mitigate cold starts

To me, serverless as it exists in 2022 is much closer to the bandit list than
the ideal list

### I have a go

There is no definitive definition of what is essentially a marketing term but we
can see some commonalities.

"serverless" is

1. no capacity planning
1. no configuration
1. no management
1. no maintenance
1. no extra work for fault tolerance,
1. no scaling of containers, VMs, or physical servers.

- Scale down to 0
- Scale up (practically) "infinitely"

1. no resources in volatile memory; computing is rather done in short bursts
   with the results persisted to storage.
1. Scale down to 0 when not required

- you pay for only what you use (for both compute and bandwidth)

1. no dependence upon teams of infrastructure and support engineers

All this sounds wonderful and maybe, just maybe a teensy bit too good to be
true. What happens when this hits the real world?

## What's in a web app

A web app is made up of:

For a true serverless experience we need to hit the serverless goals for all
these

1. File storage

- basically a solved problem - solved by S3 (and it's clones)
- modern monoliths already use S3 so this is almost a no-op

1. Data storage

- DynamoDB does this but it's a KV store and unsuitable for a lot of things
  you'll almost certainly need from your data storage
    - => you'll probably also have a relational DB
- Work is happening on making existing relational DBs more "serverless" but it's
  unclear how close that will get
- Truly serverless general purpose data storage is currently an unsolved
  problem.

1. Compute

- FaaS marketing focuses here. Ironically this has always been the easiest to
  scale
    - FaaS marketing sometimes behaves like there were no good solutions to this
      before FaaS
    - The details of FaaS marketing don't tell any lies but the general
      impression most people take away is there must have been no good solutions
      for this. This general impression is favourable to the marketing effort so
      it never challenged.
- compute is what most of the "serverless" marketing focuses on

1. Caching

- Not really serverless but can be scaled somewhat elastically
- Serverless caching is basically still the same as pre-serverless caching
- Still depends on persistent connections
- Examples
    - ElastiCache
    - Momento describe themselves as the best truly serverless cache
        - https://www.gomomento.com/blog/finally-a-serverless-cache-that-delivers-on-the-promise-of-the-cloud-era
    - AWS API Gateway can use Memcached to cache API responses for you but
      that's still ElastiCache under the hood
    - You can use DynamoDB App Accelerator which adds a layer of caching to your
      DynamoDB
- Is there a truly serverless cache solution that's good to use?

## Serverless type 1: Backend as a service

There are a number of "backend as a service" things which are very useful but
I'm going to ignore them ehre because

1. They are not For example:

- AWS Cognito
- Google Firebase
- Auth0
- Okta

2. Functions as a service

- Examples
    - AWS Lambda

These two types are lumped together under "serverless" (the marketing term) but
they are completely different.

Not much to say here

- ++ can be a great choice for a web application (no matter what architectural
  style it uses)
- -- usually vendor lock-in
- -- can be expensive. Price definitely does not scale to $0 in most cases
- -- you are stuck with what features they provide - can be difficult if (for
  example) you want to run a kind of query that the APIs do not support well
- ++ they do not dictate application architecture to your team
- ++ generally these solutions hit the criteria for serverless but also they
  aren't so much serverless as "SaaS"

## Serverless type 2: Functions as a service

This is what most people mean when they talk about serverless

### FaaS Case Study: BBC

https://www.infoq.com/news/2021/01/bbc-serverless-scale/
https://medium.com/bbc-product-technology/optimising-serverless-for-bbc-online-118fe2c04beb
https://medium.com/bbc-product-technology/bbc-online-a-year-with-serverless-ffc2ae474277

https://youtu.be/WE-tkz2tf30?t=735 discusses the issues they see with cold start
p50 140ms p99 500ms p100 multiple seconds due to cold start

BBC news is the no. 1 news site in the world (or close to no. 1)

- seem to use a single _index.php style_ lambda as entry point
- which dispatches to other lambdas to build other parts of the page
- redis used for caching throughout
- front-end traffic not via API Gateway but via EC2 instances running
  varnish/nginx/etc.
- they have a number of layers of caching in front of their lambdas

#### Pros

- Scale compute (only) to 0 is easy
- Naturally handles very spiky traffic - can be made to scale really well
    - Scaling isn't free because there will be other parts of the architecture
      which won't be so elastic e.g. database
- If you have multiple dev teams then, assuming the architecture supports it,
  they _may_ be able to overlap less and deploy more easily

#### Cons

- It only talks about compute. And infinitely elastic compute with limits on
  data storage and caching doesn't really buy you much in many cases
- Security can be more complex e.g. every lambda has an IAM policy which can be
  too broad
- Packaging complexity is a lot higher
- Deployment complexity is a lot higher
- Deployment speed could go either way
    - Could be faster if you have many teams contending for a monolith
    - But in general will probably be slower
- Dependency management will be more complex
    - Examples
        - Security patch for a lib has to be deployed into every function which
          uses it
- You need distributed monitoring, can't really rely on just reading the log
  file anymore

Serverless can be

- Slower
- Restrictive
- Expensive

#### Unclear

- Cost may or may not be better than a trad architecture - depends very much on
  use case.

#### Cost

- EC2 instances need to have quite a bit of head room anyway (e.g. 20%
  utilisation as a target) so the extra cost of serverless per request can be
  balanced against not paying for that headroom

## Known problems

- Cold starts
- Debugging
- Monitoring
- Vendor lock-in
    - Take a bunch of what Rails/Laravel/Django used to do and let AWS do it
- Databases which are truly serverless have severe tradeoffs
    - dynamodb is mostly a key-value store. you will still probably need a SQL
      DB
    - many DBs marketed as serverless are not really serverless

## How close to the serverless ideals is a standard webapp in fargate?

TODO

## Deno

- Alternative to node
- runs JS and TS
- ??? run wasm?
- built-in typescript support
- has a more "batteries included" approach than node
- built in rust
- has a "deno deploy" serverless platform
    - https://deno.com/deploy
    - 35 edge locations
    - is white labeled
        - used by Netlify **edge** functions (but not their only runtime - they
          also use AWS lambda)
        - used by Supabase **edge** functions
- has a web framework https://fresh.deno.dev/
- has better cold start vs node

## Serverless databases

### Firebase (BaaS)

- https://cloud.google.com/firestore
- has been around the longest of the BaaS offerings
- has a REST API and RPC API but no SQL support

### Supabase (BaaS)

- https://supabase.com/
- aims to be a Firebase alternative (not just a database)
    - DB + Auth + Edge functions + File storage
- can be self-hosted
- DB is based on Postgres
- No clear docs on where they host but https://status.supabase.com/#month has
  US, EU, SG (Singapore)
- You have to choose a region
    - so it's serverless but not edge

Latency has to be an issue given the storage is only in 3 regions and their edge
compute runs in 35?

### Neon (SQL)

- https://neon.tech/
- based on Postgres
    - runs postgres as compute and have their own custom storage backend
- custom backend is open source https://github.com/neondatabase/neon
- scales down to 0
- allows you to branch your DB
- still super early, pricing not announced yet (as of end 2022)
- currently only in 4 regions https://neon.tech/docs/introduction/regions/
    - singapore closest to us
    - there are plans for more regions
    - seems to be built on AWS
- You have to choose a region
    - so it's serverless but not edge

### DynamoDB

Can it sync to a SQL DB? Yes DynamoDB has streams which can (with some faff)
replicate data into RDS
https://aws.amazon.com/blogs/database/how-to-stream-data-from-amazon-dynamodb-to-amazon-aurora-using-aws-lambda-and-amazon-kinesis-firehose/

### Macrometa

- https://www.macrometa.com/
- https://blog.cloudflare.com/partnership-announcement-db/

### Fauna

- https://fauna.com/
- A nosql serverless database
- https://blog.cloudflare.com/partnership-announcement-db/

### CloudFlare KV

Can it sync to a SQL DB? Yes

## Edge compute

Compute may move to the edge but can data? And if data can't move then how
useful will compute be?

Seems to focus on having a fast KV storage option - there is currently no
serverless SQL DB which can be as distributed as edge compute currently is.

Will edge compute ever have low-latency access to a SQL DB?

Current implementations seem to focus on having a fast KV store and not tackling
the SQL DB case.

Does this mean that the KV + SQL pattern will become more of a norm? The bits of
the site which need to scale talk only to KV $something runs $somewhere which
syncs the KV with SQL storage for all the things that SQL is good at

## Misc Ramblings

Building "servicefully":

1. Outsourcing resource management
2. maximising use of hosted services

Is this akin to moving from on-prem to the cloud? Note that on-prem is still a
thing even when all we talk about is cloud Moving to cloud didn't force you to
rearchitect just to move Moving to cloud native does mean a re-architecture

This marketing makes "server code" the fucking bogey man while cheerfully
suggesting you make your whole app from React as a way to reduce overall
complexity

Do we need a "build a typical rails app in serverless" problem to reference?

does not doing serverless cut us off from the future? No. We can always walk the
path to delivering serverless Is there an advantage to being an early adopter of
it? Technical space: only if it's more efficient to build and maintain than
Rails etc. Perception space: ??? How long will serverless be pushed as "the
future". do our customers care?

Serverless architecture is not the same challenge as serverless implementation
architecture is where we have the gaps

Is there a way to use a serverless for part of an app and have the Rails app
also exist e.g. the public front-end served by serverless + (?postgres, ?dynamo,
?redis, ?something else), the backend by Rails + Postgres

What does a good serverless archtiecture for a web app look like? What data
storage to use? ? use microservices (vertical slices through the architecture)
or ?single function or ?network of functions

So it's so vague it can't be nailed down with criticism because the argument
it's hard to discuss the pros/cons because it's so vague that it's hard to be
sure everyone is working from the same definition

One meaning is that anything that is modern and good is serverless
