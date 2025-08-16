# Embeddings and Vectors

- [Embeddings and Vectors](#embeddings-and-vectors)
    - [Next steps](#next-steps)
    - [Embeddings](#embeddings)
        - [Ways of creating embeddings](#ways-of-creating-embeddings)
        - [Semantic search using OpenSearch](#semantic-search-using-opensearch)
        - [Where should semantic search be implemented?](#where-should-semantic-search-be-implemented)
        - [Storing ML models in an OpenSearch cluster](#storing-ml-models-in-an-opensearch-cluster)
        - [The OpenAI flavour](#the-openai-flavour)
        - [pgvector](#pgvector)
- [Vector storage](#vector-storage)
    - [Requirements](#requirements)
    - [Querying options](#querying-options)
    - [Options](#options)
- [Which model to use for embeddings?](#which-model-to-use-for-embeddings)

## Next steps

```
learn more about k-NN and the indexes which do approximate k-NN
learn about k-NN support in Opensearch and postgres (ignore other stores for now)
figure out which embeddings models is best
```

## Embeddings

- Embedding = any data (text, images etc.) which has been converted to an array
  of floats
- numerical representations of text where each word or phrase is represented as
  a dense vector of real numbers
- can capture semantic meanings and relationships between words or phrases,
  which enables machines to understand and process human language efficiently
- Supposed to be a representation that captures the semantic meaning of the data
- You can compare embeddings of different data types e.g. if embedding of text
  matches embedding of image then the text might descrie the image
- searches which compare embeddings come back ranked by a float 0-1 score which
  describes how well it matched
- the verb "embed" and "encode" are used interchangeably e.g. "embed my data" is
  same as "encode my data as embeddings"
- Allow you to
    - Create a sort of long-term memory for AI models
    - semantic search
    - semantic clustering
    - semantic recommendation
    - semantic classification
- Google image search breaks images up into blocks of pixels and creates an
  embedding for each block
- A database full of embeddings is often called a vector database
- Vector databases
    - SingleStore
    - pg_vector addon to Postgres
    - Elasticsearch/Opensearch (not strictly a vector DB but can store vectors)
-

### Ways of creating embeddings

1. Use OpenAI API (paid) https://platform.openai.com/docs/guides/embeddings
1. Sentence Transformers open source lib
1. Sentence Transformers via Hugging face API
   https://huggingface.co/blog/getting-started-with-embeddings

Q: is an AI model the only way to create an embedding?

### Semantic search using OpenSearch

https://opensearch.org/blog/semantic-search-solutions/

Compare the records in your DB to the query by embedding the query and comparing
it to the stored embeddings The comparison can be

- approximate nearest neighbour search e.g. k-NN

Search quality depends on the size of the NN. Large NNs learn more expressive
embeddings. A _transformer_ is a large NN

Transformer is a NN which works well across many tasks. Trained on hug amounts
of data, it encodes expressive embeddings. You can fine tune the transformer by
train it on data of the form

```
(query, relevant-passage)
```

Many fine-tuned transformer architectures are publicly available

You can choose your transformer model as

1. Pretrained
    - easier to use
2. Tuned - a Pretrained model which has been tuned with domain specific data
    - more powerful

Steps

1. Choose a model
    - OpenSearch recommends TAS-B from HuggingFace
    - TAS-B
        - creates 768-dim vectors
        - trained on the [MS Macro](https://huggingface.co/datasets/ms_marco)
          dataset
2. Make sure model is suitable for high-perf environments
3. Upload the model to the OpenSearch cluster using "model serving framework"
4. Perform k-NN searches using the model and the Open Search plugin "Neural
   Search"

You can do "hybrid search" which combines a semantic search and a lexical search
into one set of ordered results

ElasticSearch offers different NN models than OpenSearch. Unclear which is
better.

### Where should semantic search be implemented?

- Option: In OpenSearch/ElasticSearch
    - ++ Our other kinds of search already happen here e.g. keyword, fuzzy
      matching
    - ++ You can easily create hybrid searches whose results are a mix of a
      lexical and semantic search
    - ?? How limiting is it having to upload the model to the cluster? Does it
      limit me to only some models? Are these the best models?
    - ?? How to integrate with an API based model like OpenAPI one?
    - ?? What is cost per query? How slow are queries? How much hardware do we
      need to throw at the cluster? Is this an efficient or inefficient way to
      run models?
- Option: DIY, store embeddings in pg_vector
    - ++ most control, can use any model we want, can run the model locally, in
      API etc.
    - ++ embeddings are expensive to compute so we want to store them long-term
      => Postgres not OpenSearch
    - -- we are on our own for creating hybrid searches
    - -- we need to either implement other kinds of searches our selves or use
      OpenSearch
- Option: Create embeddings locally, store them in OpenSearch, use opensearch
  for searches
    - ++ we get the advantages of doing searches in OS
    - ++ we are not forced to upload the model to the cluster - this might scale
      better and give us more control over performance
    - ?? is this even possible?
        - it seems yes:
          https://blog.reactivesearch.io/knn-search-with-opensearch-and-openai-embeddings-an-in-depth-guide

### Storing ML models in an OpenSearch cluster

- Overview: https://opensearch.org/docs/latest/ml-commons-plugin/ml-framework/
- You must use the API to upload a model
    - https://opensearch.org/docs/latest/ml-commons-plugin/ml-framework/#upload-model-to-opensearch

        ```bash
        # Model upload request
        POST /_plugins/_ml/models/_upload
        {
          "name": "all-MiniLM-L6-v2",
          "version": "1.0.0",
          "description": "test model",
          "model_format": "TORCH_SCRIPT",
          "model_config": {
            "model_type": "bert",
            "embedding_dimension": 384,
            "framework_type": "sentence_transformers"
          },
          "url": "https://github.com/opensearch-project/ml-commons/raw/2.x/ml-algorithms/src/test/resources/org/opensearch/ml/engine/algorithms/text_embedding/all-MiniLM-L6-v2_torchscript_sentence-transformer.zip?raw=true"
        }

        # and response
        {
          "task_id" : "ew8I44MBhyWuIwnfvDIH",
          "status" : "CREATED"
        }
        ```

- You can manage them from dashboards
    - https://opensearch.org/docs/latest/ml-commons-plugin/ml-dashboard/
- o use a model in OpenSearch, you’ll need to export the model into a portable
  format. As of Version 2.5, OpenSearch only supports the TorchScript and ONNX
  formats. Also files must be saved as zip files before upload
    - Torchscript
        - A way replacing the dependency on the Python VM with a much smaller,
          targetted Torchscript VM
        - TorchScript is a statically typed subset of Python
        - https://pytorch.org/docs/stable/jit.html
        - Torchscript files typically have `.pt` suffix
        - > To load your serialized PyTorch model in C++, your application must
          > depend on the PyTorch C++ API – also known as LibTorch.
        - You write your model in torchscript subset of python. Then save it as
          a `.pt` file. The a C++ app can load execute that `.pt` file using
          libtorch (which is presumably some kind of interpreter/VM)
    - ONNX
        - A way replacing the dependency on the Python VM with a much smaller,
          targetted ONNX VM
        - https://onnx.ai/
        - > an open format built to represent machine learning models. ONNX
          > defines a common set of opjerators - the building blocks of machine
          > learning and deep learning models - and a common file format to
          > enable AI developers to use models with a variety of frameworks,
          > tools, runtimes, and compilers.
        - > Using ONNX in production means the prediction function of a model
          > can be implemented with ONNX operators. A runtime must be chosen,
          > one available on the platform the model is deployed.
        - Use a
          [converting library](https://onnx.ai/onnx/intro/converters.html#what-is-a-converting-library)
          to rewrite your scikit-learn, tensorflow, lightgbm, pytorch models to
          ONNX
- Most deep learning models are more than 100 MB, making it difficult to fit
  them into a single document. OpenSearch splits the model file into smaller
  chunks to be stored in a model index. When allocating machine learning (ML) or
  data nodes for your OpenSearch cluster, make sure you correctly size your ML
  nodes so that you have enough memory when making ML inferences.
- You can use GPU acceleration on your ML node(s) if available
- you can choose which nodes run your model
- Opensearch supports a fixed list of approx. 10 pre-trained models - see
  https://opensearch.org/docs/latest/ml-commons-plugin/pretrained-models/#supported-pretrained-models
    - These have been put in the right format for you by OpenSearch
    - All models can be traced back to HuggingFace models
- You can also bring your own model provided it is in a supported format

### The OpenAI flavour

https://platform.openai.com/docs/guides/embeddings

> embeddings measure the relatedness of text strings. An embedding is a vector
> (list) of floating point numbers. The distance between two vectors measures
> their relatedness. Small distances suggest high relatedness and large
> distances suggest low relatedness.

### pgvector

- Postgres extension to allow storage and querying of vectors
- https://github.com/pgvector/pgvector
- Provides operators for comparing vectors

    ```
    <-> = nearest neighbor
    <#> = negative inner product (Postgres only supports ASC order index scans on operators, multiply by -1 to get inner product)
    <=> = cosine distance


    AVG(my_vec) # get average of a single vector or a group of vectors
    ```

Indexes

- exact nearest neighbour search (the default)
    - provides perfect recall
- approximate nearest neighbour search
    - trades some recall for speed
    - requires you to add an "approximate index"

Available index types

- IVFFlat
- HNSQ

Other vector databases:

- https://aws.amazon.com/opensearch-service/serverless-vector-engine/
- https://www.pinecone.io/
- https://redis.com/

# Vector storage

Vector = array of floats

Goals

- Semantic search

## Requirements

- some way of creating embeddings (optional, can just call out to a model for
  this)
- k-NN querying

## Querying options

> Typically powered by k-NN indexes built using algorithms like Hierarchical
> Navigable Small Worlds (HNSW) and Inverted File (IVF) System, vector databases
> augment k-NN functionality by providing a foundation for applications like
> data management, fault tolerance, resource access controls, and a query
> engine.

- Exact nearest-neighbour search - perfect recall
- Approximate nearest neighbour search - trades recall for speed. Facilited by
  k-NN Index types
    - Inverted File \_???\_\_\_ (IVFFlat)
    - Heirarchical Navigable Small Worlds (HNSW)

## Options

https://www.datacamp.com/blog/the-top-5-vector-databases

- Weaviate
    - https://weaviate.io/
- Postgres + pgvector
    - https://github.com/pgvector/pgvector/
    - [supported by RDS since May 23](https://aws.amazon.com/about-aws/whats-new/2023/05/amazon-rds-postgresql-pgvector-ml-model-integration/)
      (Postgres 15.2+)
    - exact and approximate nearest neighbor search
    - L2 distance, inner product, and cosine distance
    - does not create embeddings for you
- OpenSearch
    - https://opensearch.org/platform/search/vector-database.html
- Redis

I'm just going to use Postgres, OpenSearch, Redis until I hit some compelling
reason to use something else.

# Which model to use for embeddings?

Leaderboard: https://huggingface.co/spaces/mteb/leaderboard

Cohere models do very well - https://cohere.com/

Overall winner (end 2023) is https://github.com/SeanLee97/AnglE
