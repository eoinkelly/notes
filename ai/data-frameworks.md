# Data frameworks

## LangChain

https://www.langchain.com/

## Llama Index

* https://www.llamaindex.ai/
* https://docs.llamaindex.ai/en/latest/

> LlamaIndex is a simple, flexible data framework for connecting custom data sources to large language models.

https://github.com/run-llama/llama_index
* available in python and typescript

> LlamaIndex is a "data framework" to help you build LLM apps. It provides the
> following tools:
>
> Offers data connectors to ingest your existing data sources and data formats (APIs, PDFs, docs, SQL, etc.)
> Provides ways to structure your data (indices, graphs) so that this data can be easily used with LLMs.
> Provides an advanced retrieval/query interface over your data: Feed in any LLM input prompt, get back retrieved context and knowledge-augmented output.
> Allows easy integrations with your outer application framework (e.g. with LangChain, Flask, Docker, ChatGPT, anything else).
> LlamaIndex provides tools for both beginner users and advanced users. Our high-level API allows beginner users to use LlamaIndex to ingest and query their data in 5 lines of code. Our lower-level APIs allow advanced users to customize and extend any module (data connectors, indices, retrievers, query engines, reranking modules), to fit their needs.


## Comparison

> Langchain is a more general-purpose framework that can be used to build a wide variety of applications. It provides tools for loading, processing, and indexing data, as well as for interacting with LLMs. LlamaIndex: LlamaIndex is specifically designed for building search and retrieval applications.

> While LLamaIndex focuses on bridging the gap between your data and LLMs, LangChain provides a different perspective. It offers a modular and extensible architecture that empowers developers to seamlessly combine LLMs with various data sources and services. This flexibility enables the creation of a wide range of applications that leverage the unique capabilities of LLMs.
> https://medium.com/@zahidali133/comparison-of-llamaindex-and-langchain-4900989752ac


Areas of focus

LangChain:

* Text generation
* Translation
* Question answering
* Summarization
* Classification

LlamaIndex:

* Document search and retrieval
* LLM augmentation
* Chatbots and virtual assistants
* Data analytics
* Content generation