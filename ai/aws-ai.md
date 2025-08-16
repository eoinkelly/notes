# AWS AI tooling

- [AWS AI tooling](#aws-ai-tooling)
    - [Jargon](#jargon)
        - [Generative AI](#generative-ai)
        - [Foundational models](#foundational-models)
    - [AWS CodeWhisperer](#aws-codewhisperer)
    - [AWS Bedrock](#aws-bedrock)
    - [Amazon Titan Foundation Models](#amazon-titan-foundation-models)
    - [Custom silicon](#custom-silicon)
    - [Amazon Comprehend](#amazon-comprehend)
    - [Amazon Kendra](#amazon-kendra)
    - [Amazon Lex](#amazon-lex)
    - [Amazon Polly](#amazon-polly)
    - [Amazon Rekognition](#amazon-rekognition)
    - [Amazon SageMaker](#amazon-sagemaker)
    - [Amazon Textract](#amazon-textract)

## Jargon

### Generative AI

Generative AI:

> Generative AI is a type of AI that can create new content and ideas, including
> conversations, stories, images, videos, and music.

### Foundational models

_Foundation models_ are very large models that have already been trained on vast
amounts of data and are customisable by you. Foundation models are different
from traditional ML models in:

1. Size - they are much bigger
2. General-purpose - traditional ML models are specialised but FMs are general
   purpose

FMs have a huge number of parameters which make them capable of learning complex
concepts.

Their first training is called "pre-training", then the customisation you do is
called training.

> The size and general-purpose nature of FMs make them different from
> traditional ML models, which typically perform specific tasks, like analyzing
> text for sentiment, classifying images, and forecasting trends.
>
> To give a sense for the change in scale, the largest pre-trained model in 2019
> was 330M parameters. Now, the largest models are more than 500B parameters—a
> 1,600x increase in size in just a few years.

## AWS CodeWhisperer

- AWS version of Github Copilot
- Available in IDEs and the Lambda console
- Has also been trained on Amazon code
- Supported languages: Python, Java, JavaScript, TypeScript, C#, Rust, Go, Ruby,
  Scala, Kotlin, PHP, C, C++, Shell Scripting, SQL
- Has built-in security scanning
    - Code security scans are currently supported for Python, Java, and
      JavaScript only.

> Content processed by CodeWhisperer Professional is not stored or used for
> service improvement.

> Content processed by CodeWhisperer Professional, such as code snippets,
> comments, and contents from files open in the IDE, is not stored or used to
> train the model, and therefore will never be reproduced in a code suggestion
> for another user.

## AWS Bedrock

- makes FMs from AI21 Labs, Anthropic, Stability AI, and Amazon accessible via
  an API.
- a serverless experience
- model families
    - Jurassic-2 from AI21 Labs
        - example use-case is translation
    - Claude (Anthropic)
        - conversational and text processing tasks
    - Stable diffusion (Stability AI)
        - text to image
- All models can be customised by putting labeled examples in S3
    - "as few as 20 examples is enough"
- your training examples are safe apparently
    - > since all data is encrypted and does not leave a customer's Virtual
      > Private Cloud (VPC), customers can trust that their data will remain
      > private and confidential
- In limited preview as of 2023-04-15

## Amazon Titan Foundation Models

- two LLMs
    1. A generative LLM
        - tasks: summarization, text generation, classification, open-ended Q&A,
          information extraction
    2. Embeddings LLM
        - > an embeddings LLM that translates text inputs (words, phrases or
          > possibly large units of text) into numerical representations (known
          > as embeddings) that contain the semantic meaning of the text. While
          > this LLM will not generate text, it is useful for applications like
          > personalization and search because by comparing embeddings the model
          > will produce more relevant and contextual responses than word
          > matching.

> Titan FMs are built to detect and remove harmful content in the data, reject
> inappropriate content in the user input, and filter the models' outputs that
> contain inappropriate content (such as hate speech, profanity, and violence).

## Custom silicon

- Chips
    - AWS Trainium
        - optimized for training models
    - AWS Inferentia2
        - a chip optimized for doing inference

> Trn1 instances, powered by Trainium, can deliver up to 50% savings on training
> costs over any other EC2 instance, and are optimized to distribute training
> across multiple servers connected with 800 Gbps of second-generation Elastic
> Fabric Adapter (EFA) networking

## Amazon Comprehend

- NLP service that uses ML to get "insights" out of unstructured text

> It develops insights by recognizing the entities, key phrases, language,
> sentiments, and other common elements in a document.

> You can run real-time analysis for small workloads or you can start
> asynchronous analysis jobs for large document sets. You can use the
> pre-trained models that Amazon Comprehend provides, or you can train your own
> custom models for classification and entity recognition.

> Amazon Comprehend may store your content to continuously improve the quality
> of its pre-trained models.

> Entities – Amazon Comprehend returns a list of entities, such as people,
> places, and locations, identified in a document.
>
> Events – Amazon Comprehend detects specific types of events and related
> details.
>
> Key phrases – Amazon Comprehend extracts key phrases that appear in a
> document. For example, a document about a basketball game might return the
> names of the teams, the name of the venue, and the final score.
>
> Personally identifiable information (PII) – Amazon Comprehend analyzes
> documents to detect personal data that identify an individual, such as an
> address, bank account number, or phone number.
>
> Dominant language – Amazon Comprehend identifies the dominant language in a
> document. Amazon Comprehend can identify 100 languages.
>
> Sentiment – Amazon Comprehend determines the dominant sentiment of a document.
> Sentiment can be positive, neutral, negative, or mixed.
>
> Targeted Sentiment – Amazon Comprehend determines the sentiment of specific
> entities mentioned in a document. The sentiment of each mention can be
> positive, neutral, negative, or mixed.
>
> Syntax analysis – Amazon Comprehend parses each word in your document and
> determines the part of speech for the word. For example, in the sentence "It
> is raining today in Seattle," "it" is identified as a pronoun, "raining" is
> identified as a verb, and "Seattle" is identified as a proper noun.

## Amazon Kendra

- https://aws.amazon.com/kendra/
- enterprise search service that helps you search across different content
  repositories with built-in connectors.
- lets you create a search that will search your structured and unstructured
  documents
- you can plug it into opensearch

## Amazon Lex

- Chatbots
- Pre-dates GPT & friends
- Build conversational interfaces

## Amazon Polly

- Text to speech

## Amazon Rekognition

- https://aws.amazon.com/rekognition/
- pre-trained computer vision
- facial recognition
- face compare
- face detection and analysis
- image labelling
- text detection in image
- label parts of images
- detect inappropriate content
- verify identity online

## Amazon SageMaker

- The other services provided trained models for you to do inference with (or in
  some cases customise and then do inference). Sagemaker helps you build and
  train your own models.
- Aimed at data scientists

> Build, train, and deploy machine learning models for any use case with fully
> managed infrastructure, tools, and workflows

## Amazon Textract

- https://aws.amazon.com/textract/

> Amazon Textract is a machine learning (ML) service that automatically extracts
> text, handwriting, and data from scanned documents. It goes beyond simple
> optical character recognition (OCR) to identify, understand, and extract data
> from forms and tables.
