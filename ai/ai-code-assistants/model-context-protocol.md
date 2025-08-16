# Model Context Protocol MCP

## Sources

- https://www.anthropic.com/news/model-context-protocol
- https://blog.cloudflare.com/model-context-protocol/

## Overview

- Open standard for bi-directional connecting AI systems with data sources
- Architecture of MCP servers and MCP clients
- Claude desktop app can act as MCP server
    - but I think you need to be on a plan
- MCP servers available for systems like Google Drive, Slack, GitHub, Git,
  Postgres, and Puppeteer
    - Semi-official list of server implementations available:
      https://github.com/modelcontextprotocol/servers

## Spec

- Protocol: https://github.com/modelcontextprotocol
- Schema officially defined in terms of TS types:
  https://github.com/modelcontextprotocol/specification/blob/main/schema/2024-11-05/schema.ts
- Base protocol
    - uses JSON-RPC to exchange messages between client & server
    - stateful connections
    - clients and servers can negotiate capabilities
- Takes inspiration from LSP
- Players
    - Hosts
        - LLM applications that initiate connections
        - The idea is that a host would have many clients running (one per data
          source that it interacts with)
    - Clients
        - connectors within a `Host` application (see above)
    - Servers
        - services that provide context and capabilities
- Transports
    1. stdio
    2. SSE - HTTP Server Sent Events
        - this transport provides a HTTP server with 2 endpoints:
            1. `/sse` endpoint for server sent events
            2. `/messages` endpoint for the client to send it's messages to
- Clients may offer "Sampling"
    - server initiated agentic behaviours and recursive LLM interactions
    - the client allows the server to call into the client's LLM in a controlled
      way
    - the server is assumed to not have an LLM. It can ask the client LLM for
      generations/completions to help the server generate it's output
- The intention is that users are asked to authorize every resource/tool use in
  some way but how is not specified
- Servers offer:
    - Resources
        - Lets you expose data to the LLM
        - Conceptually they are GET endpoints a Host can call
        - they have a URL associated
        - client can list resources
            - it could present the list to the user for them to select
        - each resource has a URI
    - Tools
        - Expose functions for the AI model to execute
        - Conceptually POST endpoints where LLM can send data and some
          side-effect happens
        - they have a URL associated
        - define
            - tool name
            - tool params schema
            - tool return type and value
    - Prompts
        - server can expose prompt templates to the client keyed on a name
        - prompts can take arguments (strings or images in base64
        - functions which take arguments from the client and generate a string
          intended for use as an LLM prompt
        - clients can list available prompts
        - Reusable templates for LLM interactions
- Roles
    1. `user` - the user of the application
    2. `assistant` - the LLM (I think)

## SDKs

- SDKs also available (Python and TS official, others community)
- Typescript: https://github.com/modelcontextprotocol/typescript-sdk
