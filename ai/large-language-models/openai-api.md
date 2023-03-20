# OpenAI API

-   Docs: https://platform.openai.com/docs/api-reference/introduction
-   Authentication via API key in HTTP Basic Auth header

## API endpoints overview

1. Models: List models
1. Models: Show info about a specific model
1. Completions: Create completion(s) from a given prompt
1. Chat completion: Create a chat response given a chat message
1. Edits: Given some text (prompt) and an instruction, perform the instruction to edit the prompt

    ```json
    {
        "model": "text-davinci-edit-001",
        "input": "What day of the wek is it?",
        "instruction": "Fix the spelling mistakes",
    }

    {
        "object": "edit",
        "created": 1589478378,
        "choices": [
            {
            "text": "What day of the week is it?",
            "index": 0,
            }
        ],
        "usage": {
            "prompt_tokens": 25,
            "completion_tokens": 32,
            "total_tokens": 57
        }
    }
    ```

1. Images: Generate 1-10 images (max size 1024KB each) based on the given prompt
1. Images: Edit an image
1. Images: Create variations of a given image
1. Embeddings: Create a vector representation of a given input which can be consumed by ML models
1. Audio: Create an audio transcription of a given audio file
1. Audio: Create an English text transcription of the given audio where audio is in a different language (i.e. translation)
1. Various endpoints for CRUDing files you have uploaded to OpenAI
1. Fine-tune: Create a fine-tune job for a given uploaded dataset and a given OpenAI model
1. Fine-tune: RUD existing fine-tunes
1. Moderation: Check if a given input violates OpenAIs content policy

    ```json
    {
        "input": "I want to kill them."
    }

    {
        "id": "modr-5MWoLO",
        "model": "text-moderation-001",
        "results": [
            {
            "categories": {
                "hate": false,
                "hate/threatening": true,
                "self-harm": false,
                "sexual": false,
                "sexual/minors": false,
                "violence": true,
                "violence/graphic": false
            },
            "category_scores": {
                "hate": 0.22714105248451233,
                "hate/threatening": 0.4132447838783264,
                "self-harm": 0.005232391878962517,
                "sexual": 0.01407341007143259,
                "sexual/minors": 0.0038522258400917053,
                "violence": 0.9223177433013916,
                "violence/graphic": 0.036865197122097015
            },
            "flagged": true
            }
        ]
    }
    ```
