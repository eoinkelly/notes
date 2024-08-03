# Model file formats


## File formats for storing models used for inference

https://medium.com/@phillipgimmi/what-is-gguf-and-ggml-e364834d241c

* GGML (`my-model.ggml`)
    * GPT-Generated Model Language
    * not flexible
    * deprecated
* GGUF (`my-model.gguf`)
    * Georgi Gerganov's Unified Format (probably, not clear)
    * released by Facekbook in Aug 2023
    * replaces GGML, improves it
    * extensible
    * can store more than llama models
    * Ollama supports importing any model in GGUF format in its `Modelfile`