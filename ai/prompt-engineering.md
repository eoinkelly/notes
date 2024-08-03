# Prompt engineering

- [Prompt engineering](#prompt-engineering)
  - [Takeaways](#takeaways)
  - [General rules](#general-rules)
  - [Context window sizes](#context-window-sizes)
  - [Claude](#claude)
    - [System prompt](#system-prompt)
  - [OpenAI](#openai)

## Takeaways

-   Start a new conversation if you are asking a new question - don't be lazy and just use the same conversation. The old context from unrelated questions might give you worse answers

## General rules

-   The context window for the model is presented as the "conversation" in the chatbot UI
    -   The full context window is re-parsed every time you submit some text for it to predict an output.


## Context window sizes

Assume `1 token ~= 5.5 bytes`

The average English word is around 4.5 characters long, plus a space for separation, making it 5.5 characters on average. Given that each character is typically one byte in UTF-8 encoding, this calculation gives us an average word length of about 5.5 bytes.

| Model       | context window tokens      | Approx. context window |
| ----------- | -------------------------- | ---------------------- |
| Claude 2.1  | 200k                       | 1.1 mb                 |
| GPT-4       | 8k (32k also via API only) | 44 kb                  |
| GPT-4 Turbo | 128k                       | 704 kb                 |
| Bard        | Google doesn't say         | -                      |

## Claude

https://docs.anthropic.com/claude/docs/introduction-to-prompt-design

Claude generates tokens
It uses all the text you have sent it **and** all it's previous responses in the "context window" to generate the next token
It cannot remember previous conversations
It cannot open links

Under the hood claude expects the context window to have tagged sections about which bits came from you and which came from claude. It uses `\n\nHuman:` and `\n\nAssistant:` as tokens to indicate a change in speaker (note the required newlines). The web app and slack app hide this complexity but you need to know it for the API.

Claude 2.1 can take 1.3 MB of context window

```
Human: stuff you typed

Assistant: a response from claude
```

> Claude 2.1 has double the context length, at ~150,000 words / ~200,000 tokens / ~680,000 Unicode characters

```
Bard: 680,000 Unicode characters is approximately 1328.12 KB.
GPT4: 680,000 Unicode characters, when encoded using UTF-16 (which uses 2 bytes per character), would amount to approximately 1328.125 kilobytes (KB).
    GPT4 also gave me python code to show it's work

Claude: Okay, let's calculate this step-by-step:

1) Unicode characters are encoded using variable length encoding. Each character can take up between 1 byte to 4 bytes, with the most common characters taking 1 or 2 bytes each.

2) So to get an estimate, we need to make an assumption about the average number of bytes per character. Let's assume 2 bytes per character, which is reasonable for a lot of text.

3) There are 680,000 characters.

4) If each character is 2 bytes, then 680,000 characters x 2 bytes per character = 1,360,000 bytes

5) To convert to kilobytes, divide the bytes by 1024.

So 1,360,000 bytes / 1024 bytes per kilobyte = 1,328 KB or approximately 1,328 kilobytes.

Therefore, 680,000 Unicode characters is approximately 1,328 KB or 1.3 megabytes. Let me know if you need any clarification or have an alternative average bytes per character estimate!

Winner: Claude gave the best answer, stating it's assumptions the clearest
```

### System prompt

It is allowed by the API to include text before the first \n\nHuman:; this is sometimes called a "system prompt".

A system prompt is a way of providing context and instructions to Claude, such as specifying a particular goal or role for Claude before asking it a question or giving it a task. System prompts can include:

-   Task instructions
-   Personalization, such as role prompting & tone instructions
-   Context for the user input
-   Creativity constraints & style guidance, such as being more concise
-   External knowledge & data, such as FAQ documents or guidelines
-   Rules & guardrails
-   Output verification standards, such asking for citations or thinking out loud to enhance credibility

> Your whole prompt should still be one multiline string, including the system prompt. You should still use two new lines after the system prompt, before Human:.

## OpenAI

Sources

* https://medium.com/coinmonks/top-20-chatgpt-prompts-that-every-prompt-engineers-should-know-937b0ea5472

has the idea of roles in the API messages I submit - `system`, `user`

Messages with the "system" role are not considered as part of the conversation but rather as directives or guidelines for how the model should behave or what persona it should adopt.

At the prompt you can mimic the system role by setting context at the start of a conversation. It is not as precise but mostly works


```
Summarise the text delimited by ### into a single paragraph

###
your text here
###
```

```
You will be provided with text delimited by triple quotes. If it contains a sequence of instructions, re-write those instructions in the following format:

"""
your text
"""
```