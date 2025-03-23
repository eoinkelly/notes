# Copilot

Cheatsheet: https://code.visualstudio.com/docs/copilot/copilot-vscode-features

## keyboard shortcuts

| Shortcut          | Description                                                        |
| ----------------- | ------------------------------------------------------------------ |
| `cmd-i`           | Open inline chat (when editor pane is focused)                     |
| `cmd-ctrl-i`      | Open Copilot Chat                                                  |
| `cmd-shift-i`     | Open Copilot Edits                                                 |
| `cmd-opt-shift-L` | Open quick chat                                                    |
| `cmd-enter`       | Apply suggested code changes                                       |
| `cmd-/`           | Attach file to context                                             |
| `F2`              | AI renaming helper (also available via right-click, rename-symbol) |

## Picking models

I'm using Claude 3.5 as my baseline. Live with it and get used to it. Then try others to diff for
certain tasks.

It would be neat to have a set of evaluations for code assist tools

## Overview

### Extensions

Copilot is 2 vscode extensions

1. "Github Copilot"
    - inline completions
    - 1.272.0 2025-02-20 54MB
2. "GitHub Copilot Chat"
    - chat windows
    - 0.24.1 2025-02-14 32MB

### User interfaces

Good table of differences between the interfaces:
https://code.visualstudio.com/docs/copilot/copilot-edits#_how-is-copilot-edits-different-from-copilot-chat

What UIs is copilot exposed as:

- "Chat view"
    - conversation sidebar
    - a more general-purpose chat interface for asking questions about your code or technology
      topics in general. Copilot can also provide code suggestions and generate code blocks as part
      of the chat conversation. You need to manually apply each code block to the different files in
      your project to evaluate their validity.
- "Copilot Edits view"
    - edits sidebar
    - combines the "Chat view" and "Inline chat" view
    - puts you in the context of code editing, where you start an edit session and use prompts for
      making changes to your codebase. Copilot Edits can generate and apply code changes directly
      across multiple files in your codebase. You can immediately preview the generated edits within
      the context of your code.
    - Copilot Edits also provides you a code review flow where you can easily review generated edits
      and decide to accept or discard them. Copilot Chat does not have this code review mechanism.
      In addition, you can undo past edits and roll back changes to a previous accepted state.
- "Quick chat"
    - floating box at top of screen
    - identical features to the Chat view but in a different location
- "Inline chat"
    - floating box above the line you are currently on
    - The scope of Inline Chat is limited to the editor in which it's started, so it can only
      provide code suggestions for a single file. You can also use Inline Chat to ask
      general-purpose questions.
- "Next edit suggestions"
    - still in preview

#### Chat view

> The default action when you enter a chat prompt is Send and dispatch, which includes participant
> detection. If you choose Send, the request is sent directly to Copilot Chat and it won't be
> automatically dispatched to a chat participant. You can also disable automatic participant
> detection entirely with the chat.

#### Agent mode

Consider the following criteria to choose between edit mode and agent mode:

- Edit scope: you might use edit mode if your request involves only code edits and you know the
  precise scope for the changes.
- Preview feature: agent mode is still in preview and might not work for all scenarios.
- Duration: agent mode involves multiple steps to process a request, so it might take longer to get
  a response. For example, to determine the relevant context and files to edit, determine the plan
  of action, and more.
- Non-deterministic: agent mode evaluates the outcome of the generated edits and might iterate
  multiple times. As a result, agent mode can be more non-deterministic than edit mode.
- Request quota: in agent mode, depending on the complexity of the task, one prompt might result in
  many requests to the backend.

## Participants (aka agents I think)

> Participants are experts in a particular domain, such as coding, the editor, or many other areas.
> You can tag them in any chat to scope your questions and get better answers.

- Translation: A participant allows an extension to exchange text with the chat interfaces in the
  UI.
- https://www.youtube.com/watch?v=OdW2r3raAHI
- A participant is just a typescript function in an extension that gets the text you put in your
  prompt and returns a chat response (an object wrapping text from an LLM).
- The function can do anything it wants in the middle as long as it returns a response (e.g.
  markdown string in the example in video). The function can call any model available in vscode (or
  any other model you want to support in your extension).

Aside from participants, any regular vscode extension can call into the available models

> [!INFO]
>
> Participants are not available in the edits window or inline chat windows, only in main chat
> window

Available participants:

- System participants
    1. `@workspace`: knows about the code in your workspace. use it to navigate your code base, find
       relevant classes, files, and more.
    2. `@terminal`: knows about the integrated terminal shell and its contents.
    3. `@vscode`: knows about features, settings, and apis of vs code.
    4. `@github`: knows about and has skills for github repositories, issues, prs, and more. can
       also perform web searches using the bing api.
- Participants from add-ons
    1. `@githubpr`: specialized in handling pull requests on github.
    2. `@remote-ssh`: knows about remote ssh connections and can help with related tasks.
    3. `@ruby`: expert in ruby programming language and related frameworks.

Extensions can include chat participants - search for extensions which provide a participant via
`@tag:chat-participant`

### @terminal

Use the @terminal participant in the Chat view to ask questions about the integrated terminal or
shell commands.

```sh
# Example:
@terminal list the 5 largest files in this workspace

# Use the /explain command in the Chat view to explain something from the terminal.
# Example:
@terminal /explain top shell command
```

### @vscode

Use the @vscode chat participant to ask questions about VS Code by using natural language.

```sh
#Example:
@vscode how to enable word wrapping?

# Use /runCommand with the @vscode chat participant to run a VS Code command.
@vscode /runCommand
@vscode /runCommand enable developer mode

# Use /search with the @vscode chat participant to generate a VS Code search.
@vscode /search
# Example:
@vscode /search python files without imports
```

### @github

Use the @github participant in chat to ask about issues, pull requests, and more across your
repositories. Get more information about the available GitHub skills.

TODO: try to use the vscode github ui more to test out the copilot help

```sh
# Example:
@github What are all of the open PRs assigned to me?
@github Show me the recent merged pr's from @dancing-mona
```

## Slash commands

> Chat participants can also contribute what we call slash commands, which are shortcuts to specific
> functionality. Slash commands give you access to meticulously crafted prompts for common coding
> workflows, no prompt engineering required.

I think these are just shortcuts to pre-created prompts?

| Source   | Command           | Description                                                                                                                                                                                               |
| -------- | ----------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Built-in | `/tests`          | Generate tests for all or only the selected methods and functions in the editor. The generated tests are appended in an existing tests file or a new tests file is created.                               |
| Built-in | `/setupTests`     | Get help setting up a testing framework for your code. Get recommendation for a relevant testing framework, steps to set up and configure it, and suggestions for VS Code testing extensions.             |
| Built-in | `/fixTestFailure` | Ask Copilot for suggestions on how to fix failing tests.                                                                                                                                                  |
| Built-in | `/docs`           | Generate documentation comments for all or only the selected methods and functions in the editor.                                                                                                         |
| Built-in | `/fix`            | Ask Copilot for suggestions on how to fix a block of code or how to resolve any compiler or linting errors in your code. For example, to help fix unresolved Node.js package names.                       |
| Built-in | `/startDebugging` | (Experimental) Generate a launch.json debug configuration file and start a debugging session from the Chat view.                                                                                          |
| Built-in | `copilot-debug`   | Terminal command to help you debug your programs. Prefix a run command to start a debugging session for it (for example, copilot-debug python foo.py).                                                    |
| Built-in | `/new`            | Use the /new command in the Chat view to scaffold a new project or a new file. Use natural language to describe the type of project/file you need, and preview the scaffolded content before creating it. |
| Built-in | `/newNotebook`    | Use the /newNotebook command in the Chat view to generate a new Jupyter notebook based on your requirements. Use natural language to describe what the notebook should contain.                           |
| @ruby    | `/design`         | Custom Ruby command for design assistance (incomplete information)                                                                                                                                        |

### Context Variables

Variables enable you to reference specific information in your code, the editor, or information from
other extensions.

Combine these with participants to easily pass all the necessary context to Copilot.

From chat:

| Context Variable            | Description                                                                                                                   |
| --------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| #changes                    | The list of source control changes                                                                                            |
| #codebase                   | Add relevant workspace content as context to your prompt                                                                      |
| #editor                     | Add the visible contents of the active editor as context for your prompt (only what you can see on screen, not the full file) |
| #selection                  | Add the current editor selection as context to your prompt                                                                    |
| #terminalselection          | Add the current terminal selection as context to your chat prompt                                                             |
| #terminallastcommand        | Add the last run terminal command as context to your chat prompt                                                              |
| #vscodeapi                  | Add the VS Code API as context to your prompt to ask questions related to VS Code extension development                       |
| #file                       | Open a quick pick to select a file from your workspace and add it as context for your prompt                                  |
| `#<filename>`               | Type #, followed by a filename, to get filename suggestions for workspace files and attach as context                         |
| #sym                        | Open a quick pick to select a symbol from your workspace and add it as context for your prompt                                |
| `#<symbol>`                 | Type #, followed by a symbol name, to get symbol suggestions for workspace files and attach as context                        |
| #folder                     | Add all files in folder                                                                                                       |
| #problems                   | Add problems from the Problems panel                                                                                          |
| drag & drop file            | Drag & drop a file or editor onto the chat to attach the file as context                                                      |
| recent files (experimental) | Automatically include recently opened and edited files in your chat prompt                                                    |

From quick pick:

Quickly add multiple files as context by using the right arrow key in the Quick Pick

## Extras

### Generating PR titles and commit messages

Commit Generate a commit message for the current changes in a source control commit. Pull request
Generate a pull request title and description that correspond with the changes in your pull request.

### Semantic search

Semantic search Include search results from Copilot in the Search view that are semantically
relevant.

Setting: `github.copilot.chat.experimental.generateTests.codeLens`

If test coverage information is available, GitHub Copilot can offer a CodeLens Generate tests using
Copilot for functions and methods that are not yet covered by tests.

#### Indexes: local basic, local, remote

> Copilot automatically builds an advanced local index if your project has under 750 indexable
> files. For projects with between 750 and 2500 files, you can run the Build local workspace index
> command to start indexing. This command only needs to be run once.

the remote and local index status can be seen from the little `{}` menu at bottom of copilot

> It may take some time to build the initial local index or update the index if many files have
> changed (such as when switching git branches). You can monitor the current local index status in
> the workspace index status UI.

## Evaluation Tasks for LLMs like GitHub Copilot

Things I want to evaluate:

- How "helpful" was the LLM?
    - For each task
        - How much faster did it make getting to the same level of output quality?
        - Was the output quality better than would likely happen by hand?
        - How did the dev "feel" about the workflow?
    - have tasks at different "levels"
- What are the best ways to use copilot to get our work done faster?
    - Learn from each other
    - Set things up so copilot can be more successful

Why bother comparing them?

What are the "task levels" we can use copilot at?

### Green field path

```
<highest>
create whole app layer
    given description of an app in a readme, create it
create whole feature layer
    given a good jira ticket like description, how close to a fully working feature of code we are happy to maintain do we get?
create whole class/module/file layer
    given description of what class/module should do, create it and its tests
    evaluate how long it took to get to an acceptable answer
create whole functions layer
    create high quality, fully tested implementation of a function using mostly prompts
    evaluate how long it took to get to an acceptable answer
create code chunks layer
    copilot next edit suggestion
    copilot autocomplete
    regular autocomplete
<lowest>
```

### Brown field path

```
<highest>
?? other layers
create code chunks layer
    copilot next edit suggestion
    copilot autocomplete
    regular autocomplete
<lowest>
```

Here's a list of coding tasks that can effectively evaluate the capabilities of code-focused LLMs:

the brown field path is more complex and more common/useful.

1. **Bug fixing**

    - Identify and fix syntax errors
    - Debug logical errors in algorithms
    - Fix security vulnerabilities (e.g., SQL injection, XSS)

2. **Code completion**

    - Complete function bodies from signatures and comments
    - Suggest appropriate imports/dependencies
    - Generate appropriate error handling

3. **Code refactoring**

    - Convert imperative code to functional style
    - Improve code efficiency
    - Reduce complexity (e.g., simplify nested loops)

4. **Test generation**

    - Write unit tests for given functions
    - Generate comprehensive test cases covering edge cases
    - Create mocks and test fixtures

5. **Documentation**

    - Generate docstrings/comments for undocumented code
    - Create API documentation
    - Write README.md files

6. **Algorithm implementation**

    - Implement classic algorithms (sorting, searching, etc.)
    - Solve competitive programming problems
    - Optimize existing algorithms

7. **Cross-language translation**

    - Convert code between languages (e.g., Python to JavaScript)
    - Maintain functionality while using language-specific idioms

8. **API usage**

    - Generate code using external libraries/frameworks
    - Demonstrate proper use of design patterns
    - Create RESTful API endpoints

9. **Code explanation**

    - Explain complex code segments
    - Identify design patterns in existing code
    - Analyze time/space complexity

10. **Project scaffolding**

    - Generate boilerplate for specific frameworks
    - Set up project structures with appropriate files
    - Configure build tools and dependencies

11. **Database interactions**

    - Write efficient SQL queries
    - Generate ORM models and queries
    - Implement data migration scripts

12. **Asynchronous programming**
    - Convert synchronous code to asynchronous
    - Handle promises, callbacks, or async/await correctly
    - Implement concurrent operations safely

## Prompt strategies

### Q & A prompt

```
@workspace propose some structure for my project.
Ask me a series of yes/no questions that will let you provide a better recommendation
```

### Pros & cons prompt

```
what are a few ways to implement ____. Give the the pros and cons of each way
#file<file relevant to your question>
```

### Stepwise chain of thought prompt

```
help me refactor the code in #file:___
Move one step at a time.
Do not move to the next step until I give the keyword "next". Begin.
```

### Role prompt

```
You are a skilled instructor who makes complex topics easy to understand.
You come up with fun exercises so that your students can learn by doing.
Your goal is to teach students to be proficient in regex.
Move one step at a time and wait for the student to provide the correct answer before you move on to the next concept.
If the student provides the wrong answer, give them a hint.
Begin.
```

### Ask N times

Ask for the solution more than once It will be different each time which will help you explore the
space

### Ask for N solutions

Ask for 3 solutions Ask for 2 solutions so you can easily diff it

### Notice names

Notice names in the code produced - might help you understand the domain better
