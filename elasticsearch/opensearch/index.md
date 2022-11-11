# AWS OpenSearch

* Overview
  * OpenSearch forked at
    * ElasticSearch 7.10.2
    * Kibana 7.10.2
  * They are drifting further apart especially Kibana vs Dashboards
* ElasticSearch
  * https://github.com/elastic/elasticsearch-ruby
  * `gem install elasticsearch`
  * Maintained by Elastic
  * Client compatibility story
    * Language clients are forward compatible; meaning that clients support communicating with greater or equal **minor** versions of Elasticsearch.
      * e.g. elasticsearch gem 7.10 will communicate with server 7.10 or 7.14 (or later) but not 7.9
    * => Don't let you client lib major.minor version get ahead of your server
    * > Elasticsearch language clients are only backwards compatible with default distributions and without guarantees made.
* OpenSearch
  * https://github.com/opensearch-project/opensearch-ruby
  * `gem install opensearch-ruby`
  * Forked by AWS - https://aws.amazon.com/blogs/opensource/keeping-clients-of-opensearch-and-elasticsearch-compatible-with-open-source/
  * Maintained by "the community" but not sure if that's mostly AWS
  * Client compatibility story
    * https://opensearch.org/docs/latest/clients/index/
    * Opensearch clients (e.g. ruby client) are compatible with the same **major** version of OpenSearch
    * They recommend upgrading your client to have same version as the server
    * If you have to use the `elasticsearch` gem they recommend 7.13.0 (but they prefer you to use the opensearch gem)



From https://dattell.com/data-architecture-blog/opensearch-vs-elasticsearch :

> With OpenSearch originating as a fork from Elasticsearch, the two databases
> can appear to be near-identical to the unacquainted.  However, they are unique,
> becoming more so with each new update.

> OpenSearch includes access control for centralized user management, including
> LDAP and OpenID.  With Elasticsearch, you need to pay for the premium license to
> get this critical feature.

> Basically, the full suite of security features you will likely need are
> available at the Elasticsearch premium level.  Whereas, you get them for free
> with OpenSearch.

> OpenSearch Dashboards includes all of the visualization capabilities available
> in Kibana version 7.10.2.

> ... Grafana is a fork from Kibana and rather than being a duplicate service,
> it instead offers unique benefits.  Similarly, OpenSearch Dashboards is moving
> in the direction of becoming a standalone tool apart from OpenSearch.  This way
> it’s both useful as a companion to OpenSearch and also alongside other databases

> OpenSearch has been turned into a service offering from Oracle and of course
> AWS.  And there are a number of managed services and consulting support Partners
> (https://opensearch.org/partners) listed on the OpenSearch website, including
> Dattell.


From https://aws.plainenglish.io/the-difference-between-elasticsearch-open-distro-and-opensearch-d43c9a2c31b1

> The team working on OpenSearch has forked version 7.10 of Elasticsearch and is
> in the process of gutting it.

> It’s safe to say that while OpenSearch is very similar to Elasticsearch now,
> they are staring down very different paths. OpenSearch is committed to keeping
> its fork open source and has the backing of Amazon to do so. That’s why I
> believe that everyone will start to make their move over to OpenSearch

