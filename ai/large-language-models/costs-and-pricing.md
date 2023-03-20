


## Pricing

* OpenAI prices per token (piece of a word)
* https://openai.com/pricing
* 1000 tokens ~= 750 words

### Sceario: GPT4 with up to 8K of context

?? does that mean 8K of initially provided context or it remembers 8K of chat history?

    Every 4KB of prompt text we send costs USD $0.03
    Every 4KB of completion text we receive costs USD $ 0.06

    OR

    Every 400KB of prompt text we send costs USD $3.00
    Every 400KB of completion text we receive costs USD $ 6.00

If the context goes over 8K then the costs above double.

## Risks

> We spent 6 months making GPT-4 safer and more aligned. GPT-4 is 82% less
> likely to respond to requests for disallowed content and 40% more likely to
> produce factual responses than GPT-3.5 on our internal evaluations.
> https://openai.com/product/gpt-4

82% less likely is miles away from "will not" respond to requests for disallowed content


Multiple models, each with different capabilities and price points. Prices are per 1,000 tokens. You can think of tokens as pieces of words, where 1,000 tokens is about 750 words. This paragraph is 35 tokens.

## Tokenisation (which determines pricing)

    1 token ~ 4 characters ~ 4 bytes (assuming English)

 > As a rough rule of thumb, 1 token is approximately 4 characters or 0.75 words for English text.
 > https://platform.openai.com/docs/introduction/key-concepts#:~:text=As%20a%20rough%20rule%20of%20thumb%2C%201%20token%20is%20approximately%204%20characters%20or%200.75%20words%20for%20English%20text.


