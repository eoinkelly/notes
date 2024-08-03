Ways to intereact

As a prompter
writing prompts and uploading/downloading files in a UI, voice also possible now

As a developer
hitting the API endpoints provided by

Layers of what you can hit

The LLM itself e.g. "The generic ChatGPT4 provided by OpenAI"
A customised copy of the LLM
Chat GPT "apps" are tweaked via UI
More detailed: train via few shot training
Yet more detailed: tweak the training of an AWS foundation model

Does anybody train a model from scratch for production use? Seems not?

```
Q: what are model cards?
    claude 2: https://www-files.anthropic.com/production/images/ModelCardClaude2_with_appendix.pdf?dm=1700589594
```

?? rank each model based on features? or rank each company based on what problems they address?

# AI taxonomy

- [AI taxonomy](#ai-taxonomy)
  - [Model vendor: OpenAI](#model-vendor-openai)
    - [Company](#company)
    - [Available models](#available-models)
    - [Builder products](#builder-products)
    - [Chatbot product](#chatbot-product)
    - [Integrations with other products](#integrations-with-other-products)
  - [Model vendor: Anthropic](#model-vendor-anthropic)
    - [Company](#company-1)
    - [Available models](#available-models-1)
    - [Builder products](#builder-products-1)
    - [Chatbot product](#chatbot-product-1)
    - [Integrations with other products](#integrations-with-other-products-1)
  - [Model vendor: Google](#model-vendor-google)
    - [Company](#company-2)
    - [Available models](#available-models-2)
    - [Builder products](#builder-products-2)
    - [Chatbot product](#chatbot-product-2)
    - [Integrations with other products](#integrations-with-other-products-2)
  - [Model vendor: Facebook/Meta](#model-vendor-facebookmeta)
    - [Company](#company-3)
    - [Available models](#available-models-3)
    - [Builder products](#builder-products-3)
    - [Chatbot product](#chatbot-product-3)
    - [Integrations with other products](#integrations-with-other-products-3)
  - [Model vendor: Cohere](#model-vendor-cohere)
    - [Company](#company-4)
    - [Available models](#available-models-4)
    - [Builder products](#builder-products-4)
    - [Chatbot product](#chatbot-product-4)
    - [Integrations with other products](#integrations-with-other-products-4)
- [Other tools](#other-tools)
  - [Hugging Face](#hugging-face)

## Model vendor: OpenAI

### Company

### Available models

1. GPT-4
    - A set of models that improve on GPT-3.5 and can understand as well as generate natural language or code
1. GPT-4 Turbo
    - GTP-4 but larger 128k context window
1. GPT-3.5
    - A set of models that improve on GPT-3 and can understand as well as generate natural language or code
1. GPT-3.5 Turbo
    - GTP-3.5 but larger 16k context window
1. DALL·E
    - A model that can generate and edit images given a natural language prompt
1. TTS
    - A set of models that can convert text into natural sounding spoken audio
1. Whisper
    - A model that can convert audio into text
    - Open source but paid version runs faster
1. Embeddings
    - A set of models that can convert text into a numerical form
1. Moderation
    - A fine-tuned model that can detect whether text may be sensitive or unsafe
1. GPT base
    - A set of models without instruction following that can understand as well as generate natural language or code

### Builder products

### Chatbot product

### Integrations with other products

-   OpenAI is the basis for Microsoft Copilot and Github copilot

## Model vendor: Anthropic

### Company

-   Model versioning policy: ???
-   Customise behaviour only by prompting
-   Patrons: Amazon (1.25bn), Google (2bn)
-   Funding (circa end 2023): 3.3bn

### Available models

1. Claude
    - 200k tokens (~150k words) context window
    - available in AWS Bedrock as a fully managed service
    - Fine-tuning only by request for enterprise customers or now through AWS Bedrock
    - cannot do embeddings but they recommend open source SBERT
    - comment: AWS Bedrock helps fill out a number of gaps in Claude offering vs ChatGPT
1. Claude Instant
    - faster, cheaper, less accurate

### Builder products

-   You have to submit a "business interest" form to use Claude directly OR you can use AWS Bedrock. Bedrock seems to be the main way you can use Claude in a product.

### Chatbot product

-   URL: https://claude.ai/chat/
-   File uploads: Yes
-   Login via login code to email only. Bah.
-   Speech input: No
-   needs sign-up (need to provide phone number too :-( )
-   Versions
    -   Free
    -   Pro
        -   $20 USD/month
        -   Higher priority, faster access to new features, more usage credits
    -   They call it an "open beta"

### Integrations with other products

## Model vendor: Google

### Company

### Available models

1. Gemini
    - Available in multiple sizes:
        - Ultra
        - Pro
            - Chatbot runs this circa end 2023
        - Nano
            - On device
    - You cannot build it into apps yet (coming mid-dec 2023 apparently)
    - It is fully multi-model so it competes with all OpenAI offerings e.g. Whisper
2. Foundation models (https://ai.google/discover/foundation-models/)
    1. PaLM for Text
    2. PaLM for Chat
    3. Imagen family
        1. Imagen
            - > is a text-to-image model with a high degree of photorealism and deep language representations.
        2. Parti
            - > The Pathways Autoregressive Text-to-Image model (Parti), is an autoregressive text-to-image generation model that achieves high-fidelity photorealistic image generation and supports content-rich synthesis involving complex compositions and world knowledge.
        3. Muse
            - > text-to-image Transformer
    4. Codey
        - > Codey is our family of foundational coding models built on PaLM 2. Codey was fine-tuned on a large dataset of high quality, permissively licensed code from external sources and includes support for 20+ coding languages, including Python, Java, Javascript, Go, and others.
    5. Chrip
        - > Google's family of state of the art Universal Speech Models trained on 12 million hours of speech to enable automatic speech recognition (ASR) for 100+ languages

### Builder products

-   Google cloud has a [model garden](https://cloud.google.com/model-garden?hl=en)
    -   100+ models
    -   some from Google, some open source, also Anthropic Claude 2 (coming soon)
-   it also provides "Pre trained APIs" which are higher-level and are APIs built using the models

-   Google AI Studio
    -   https://cloud.google.com/generative-ai-studio?hl=en
    -   Tweak foundation models
    -   Connect the model to real-world data and real-time actions
-   Google Cloud Vertex AI
    -   https://cloud.google.com/vertex-ai?hl=en

### Chatbot product

-   Their interactive product is Bard
    -   Now based on their Gemini Pro model
    -   Requires google account sign-in (unsurprisingly)
    -   Admins must enable access for workspace accounts
    -   -- You can upload images but not other kinds of files

### Integrations with other products

## Model vendor: Facebook/Meta

### Company

### Available models

-   Llama 2
    -   open source
- Emu
  - behind their https://imagine.meta.com/

### Builder products

### Chatbot product

### Integrations with other products

## Model vendor: Cohere

### Company

https://cohere.com/

### Available models

- Coral
- Command
    - https://cohere.com/models/command
- Embed
    - https://cohere.com/models/embed
    - scores very well when compared to other embedding generation models

### Builder products

### Chatbot product

### Integrations with other products

```template
## Model vendor:

### Company

### Available models

### Builder products

### Chatbot product

### Integrations with other products
```

# Other tools

## Hugging Face

https://en.wikipedia.org/wiki/Hugging_Face

They provide

-   a python library which implements transformers
-   centralised web hosting for
    -   git repos (PRs etc.) - competes with GithHub
    -   models (with git version control)
    -   datasets
    -   running small scale web applications e.g. demos - these are called _Spaces_
        -   > We currently support two awesome Python SDKs (Gradio and Streamlit) that let you build cool apps in a matter of minutes. Users can also create static Spaces which are simple HTML/CSS/JavaScript page within a Space.
    -   BLOOM model via AWS Foundation Models (heavily partner w. AWS on this)
