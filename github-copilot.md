# Copilot

Cheatsheet: https://code.visualstudio.com/docs/copilot/copilot-vscode-features

## keyboard shortcuts

```
cmi-i = open inline chat (but only when your editor window pane is focused)
cmd-opt-shift-L = open quick chat
cmd-ctrl-i = open Copilot Chat
cmd-shift-i = open Copilot Edits

cmd-enter = apply suggested code changes

cmd-/ = attach file to context

F2 = AI renaming helper (also right-click, rename-symbol brings up same menu)
```

## Questions

- ?? seems you can't use participants in the edits UI?
- ?? how do i evaluate which models are better?

> The default action when you enter a chat prompt is Send and dispatch, which includes participant
> detection. If you choose Send, the request is sent directly to Copilot Chat and it won't be
> automatically dispatched to a chat participant. You can also disable automatic participant
> detection entirely with the chat.

Indexing

> Copilot automatically builds an advanced local index if your project has under 750 indexable
> files. For projects with between 750 and 2500 files, you can run the Build local workspace index
> command to start indexing. This command only needs to be run once.

the remote and local index status can be seen from the little `{}` menu at bottom of copilot

> It may take some time to build the initial local index or update the index if many files have
> changed (such as when switching git branches). You can monitor the current local index status in
> the workspace index status UI.

## Overview

Copilot is 2 vscode extensions

1. "Github Copilot"
    - inline completions
    - 1.272.0 2025-02-20 54MB
2. "GitHub Copilot Chat"
    - chat windows
    - 0.24.1 2025-02-14 32MB

What UIs is copilot exposed as:

- "Chat view"
    - conversation sidebar
- "Copilot Edits view"
    - edits sidebar
    - combines the "Chat view" and "Inline chat" view
- "Quick chat"
    - floating box at top of screen
- "Inline chat"
    - floating box above the line you are currently on
- "Next edit suggestions"
    - still in preview

## Participants (aka agents I think)

Participants are experts in a particular domain, such as coding, the editor, or many other areas.
You can tag them in any chat to scope your questions and get better answers.

> [!INFO]
>
> Participants are not available in the edits window or inline chat windows, only in main chat
> window

TODO: what is a participant in LLM terms? are they just a tweaked version of the model? or ust a
system prompt?

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

Available built-in slash commands:

```

/help /clear /fix /doc /tests

```

Slash commands which come from extensions:

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
| #<filename>                 | Type #, followed by a filename, to get filename suggestions for workspace files and attach as context                         |
| #sym                        | Open a quick pick to select a symbol from your workspace and add it as context for your prompt                                |
| #<symbol>                   | Type #, followed by a symbol name, to get symbol suggestions for workspace files and attach as context                        |
| #folder                     | Add all files in folder                                                                                                       |
| #problems                   | Add problems from the Problems panel                                                                                          |
| drag & drop file            | Drag & drop a file or editor onto the chat to attach the file as context                                                      |
| recent files (experimental) | Automatically include recently opened and edited files in your chat prompt                                                    |

From quick pick:

Quickly add multiple files as context by using the right arrow key in the Quick Pick

## Extras

### Chat tool

what is this?

> This extension contributes the #websearch chat tool as well which is similar to the participant
> but is useful for providing context from the web in other chat participants. For example:
> @workspace /new #websearch create a new web app written in Python using the most popular framework
> Additionally, if you are working on your own Chat Particpant or Tool, you can consume this Chat
> Tool via the vscode.lm.invokeTool API

### Generating PR titles and commit messages

Commit Generate a commit message for the current changes in a source control commit. Pull request
Generate a pull request title and description that correspond with the changes in your pull request.

### Semantic search

Semantic search Include search results from Copilot in the Search view that are semantically
relevant.

Setting: github.copilot.chat.experimental.generateTests.codeLens

If test coverage information is available, GitHub Copilot can offer a CodeLens Generate tests using
Copilot for functions and methods that are not yet covered by tests.
