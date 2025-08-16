# LangChain

https://github.com/langchain-ai/langchain

- A framework for creating aps using LLMS (<-- important, focuses on LLMs only)
- Allows you to compose AI tools to create apps
- Parts of LangChain
    - JS and Python libraries
    - REST API server
    - LangSmith
        - developer platform
        - startup charges for this
        - provides monitoring, tracing, debugging sugar
    - Templates
        - reference architectures for a variety of tasks
- Started as OSS project, evolved into a startup
- > In April 2023, LangChain had incorporated and the new startup raised over
  > $20 million in funding at a valuation of at least $200 million from venture
  > firm Sequoia Capital, a week after announcing a $10 million seed investment
  > from Benchmark.
-

## Component types

LangChain includes a collection of off-the-shelf chains of components for doing
higher level tasks. Components are **composable**, they band-aid over the
differences in the underlying tools

1. Model I/O
    - prompt management, optimization
    - a generic interface for all LLMs
    - common utilities for working with LLMs
2. Retrieval
    - chains that first fetch data from an external source for use in the
      generation step
3. Agents
    - LLM makes decision about what action to take, takes the action, observes
      result and repeats as required to achieve a whole task
