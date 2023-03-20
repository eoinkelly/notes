# ChatGPT

## Aside: Large language models

-   Uses patterns in language to build a probabilistic model of how words and phases are used _in context_.
-   Examples
    -   GPT-3: Generative Pre-trained Transformer 3
    -   BERT: Bidirectional Encoder Representations from Transformers
-   Computing power for inference depends on model size
    -

# Aside: GPT-3

-   175 **billion** parameters
-   Training was parallelized across 100's of GPUs and "other specialised hardware"

> would require 355 years and $4,600,000 to train on a [Tesla v100 instance](https://lambdalabs.com/service/gpu-cloud) > https://lambdalabs.com/blog/demystifying-gpt-3
>
> it remains a question of whether the model has learned to do reasoning, or simply memorizes training examples in a more intelligent way
> GPT-3 comes in eight sizes, ranging from 125M to 175B parameters

> All GPT-3 models use the same attention-based architecture as their GPT-2 predecessor. The smallest GPT-3 model (125M) has 12 attention layers, each with 12x 64-dimension heads. The largest GPT-3 model (175B) uses 96 attention layers, each with 96x 128-dimension heads.
>
> GPT-3 expanded the capacity of its GPT-2 by three orders of magnitudes without significant modification of the model architecture — just more layers, wider layers, and more data to train it on.
>
> Neural Networks are compressed/compiled version of the training data, the size of the dataset has to scale accordingly with the size of the model. GPT-3 175B is trained with 499 Billion tokens.

> We are waiting for OpenAI to reveal more details about the training
> infrastructure and model implementation. But to put things into perspective,
> GPT-3 175B model required 3.14E23 FLOPS of computing for training. Even at
> theoretical 28 TFLOPS for V100 and lowest 3 year reserved cloud pricing we could
> find, this will take 355 GPU-years and cost $4.6M for a single training run.
> Similarly, a single RTX 8000, assuming 15 TFLOPS, would take 665 years to run

> Time is not the only enemy. The 175 Billion parameters needs 175×4=700GB
> memory to store in FP32 (each parameter needs 4 Bytes). This is one order of
> magnitude larger than the maximum memory in a single GPU (48 GB of Quadro RTX
> 8000). To train the larger models without running out of memory, the OpenAI team
> uses a mixture of model parallelism within each matrix multiply and model
> parallelism across the layers of the network. All models were trained on V100
> GPU’s on the part of a high-bandwidth cluster provided by Microsoft.

> Other language models, such as BERT or transformerXL, need to be fine-tuned for downstream tasks. For example, to use BERT for sentiment classification or QA, one needs to incorporate additional layers that run on top of the sentence encodings. Since we need one model per task, the solution is not plug-and-play.

> However, this is not the case for GPT models. GPT uses a single model for all downstream tasks.

> It uses a paradigm which allows zero, one, or a few examples to be prefixed to the input of the model. For example, in the few-shot scenario, the model is presented with a task description, a few dozen examples, and a prompt. GPT-3 then takes all this information as the context and start to predict output token by token. The situation is similar to zero-shot and one-shot; only the number of examples are reduced.

> Let's use the task of English to French translation as a concrete example: the task description can be the sentence "Translation English to French." The few dozen examples may include text such as "sea otter => loutre de mer" and "peppermint => menthe poivree" etc. The prompt is the Enligsh word to be translated, for example, "cheese => ." Then the model is expected to output the French word for cheese, which is "fromage."

> This is where OpenAI's real ambition lies: having a model to do just about anything by conditioning it with a few examples.
>
> Another interesting view is that these examples function as "filters" that let the model search for highly relevant context or patterns from the dataset. This is possible because the dataset is practically compressed into the weights of the network. Examples that have a strong response to the filters are then "interpolated" to produce the output of the model. Obviously, the more examples you give to the model, the more precise the filter becomes, and in consequence, the better the results.

> At this stage, I found the second explanation probably makes more sense. Language models are designed to generate readable texts. They do not have a deep "understanding" of the physical world, nor are they trained to do sophisticated reasoning.

> compares human brain with where we are with the language models: A typical human brain has over 100 trillion synapses, which is another three orders of magnitudes larger than the GPT-3 175B model. Given it takes OpenAI just about a year and a quarter to increase their GPT model capacity by two orders of magnitude from 1.5B to 175B, having models with trillions of weight suddenly looks promising

### Resource usage

According to https://developer.nvidia.com/blog/deploying-a-1-3b-gpt-3-model-with-nvidia-nemo-megatron/ you can do inference on a 1.3B parameter GPT-3 model on a fiarly normal system with

> An NVIDIA Ampere architecture GPU or newer with at least 8 GB of GPU memory

### https://www.youtube.com/watch?v=SY5PvZrJhLE&t=1570s

Explains the GPT-3 paper from OpenAI.

## Aside: Lambda stack

-   A cloud focused on AI and ML use cases - instances are very GPU focused, can have multiple GPUs attached
    -   All GPU options are NVIDIA
        -   NVIDIA A100
            -   https://www.nvidia.com/en-us/data-center/a100/
        -   Tesla V100
            -   https://www.nvidia.com/en-us/data-center/v100/
            -   GPU designed for AI work
        -   Quadro RTX 6000
            -   https://www.nvidia.com/en-us/design-visualization/rtx-6000/
        -   RTX A6000
-   instances run Ubuntu with a bunch of AI/ML stuff pre-installed
    -   TensorFlow
    -   PyTorch
    -   Jupyter
    -   Keras
    -   Caffe
-   they also sell a laptop and desktop (with beefy GPU) with same software stack
-   you can connect to the instance via ssh or via Jupyter notebook
-   https://lambdalabs.com/lambda-stack-deep-learning-software

Q: Is NVIDIA _the_ choice for AI work these days?

## Aside: Transformer network

-   Underlies many LLMs
-   Transformers introduced by a [Google paper from 2017](https://arxiv.org/abs/1706.03762)

## Aside: https://huggingface.co/

-   a repository of models, datasets,
-   Has free tier
-   You can pay to host on their managed inference endpoints
-   sort of a "docker hub" thing for AI stuff

## Aside: NVIDIA NeMo Megatron

-   Great name.
-   https://developer.nvidia.com/nemo/megatron?nvid=nv-int-tblg-133070#cid=dl28_nv-int-tblg_en-us
-   A framework for training and deploying LLMs (seems to beLLM focused)
-   Seems to have some optimisation steps that allow large models to be run on lesser hardware
    -   Transforms the model file from one format to another which somehow yields speedups


## https://writings.stephenwolfram.com/2023/02/what-is-chatgpt-doing-and-why-does-it-work/

* ChatGPT tries to provide a "reasonable continuation" of whatever text it's go so far
* You
* It works in "tokens" not words
  * A token can be part of a word
* Given an existing run of tokens it is trying to figure out the next token
* It feeds the existing tokens into it's nerural network and out opos aa ranked list of possible next tokens, each with a probability assigned
* It does not always choose the highest probability token - there is a "termperature" factor (range: 0 -> 1) which controls how often it picks the most probable next token
  * 0.8 is a reasonable choice for temperature for writing essays
  * temperature is not based on any theory - it comes from what works in practice.