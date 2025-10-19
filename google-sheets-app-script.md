# Google sheets and app script


how are they integrated
what does app script do outside of sheets

Apps script is organised into projects


## Projects

> A script project represents a collection of files and resources in Google Apps Script, sometimes referred to simply as "a script". A script project has one or more script files which can either be code files (having a .gs extension) or HTML files (a .html extension). You can also include JavaScript and CSS in HTML files.
>
> The script editor always has one and only one project opened at any given time. You can open multiple projects in multiple browser windows or tabs.

- Projects can be "Standalone" or "Container bound"
    - Create "Standalone by starting at https://script.google.com/home or use clasp tool
    - Create "Container bound" by starting via Extensions > Apps Script menu in the container document
        - **When you click Extensions > Apps Script a new project is automatically created!**
- Projects use Google Cloud Platform to manage authorizations, advanced services, and other details.
- Projects have a unique ID
- Projects can be shared with other google accounts

- Every Apps Script project uses one underlying GCP Project.
    - > Your script project can use a default project that Apps Script automatically creates, or a standard project that you create yourself. In general, default projects are good for everyday or simple scripts, but you should use a standard project for any application that is complex, commercial quality, or that you intend to publish.
    - If unconfigured, it used one called "Default"
    - Logs go to GCP logs (unless you disable it)

A script (I think meaning project) can have properties defined

Are connected to a spreadsheet via triggers
The spreadsheet is called a "triggering container"
The spreadsheet is called the "container" for the Apps Script project
The sheet name and Apps Script project name can be different - they are 2 things linked, not the same thing

## Gdrive

Projects that are not started from a spreadsheet are visible in Google Drive
But you can't search by type using the regular dropdown so they are a bit hidden
You can see all your projects at https://script.google.com/home

## Modifying Apps Script projects from outside

- It has an API (disabled by default) that can allow editing of the Apps Script project from outside the usual Web IDE

## Deployments

??
- `Head`
    - ?? the deployment you get when you just hit save?
- Others?

An Apps Script project deployment is a version of the script that is made available for use as a

1. web app,
2. add-on,
3. or API executable.

By creating and managing deployments, you can iterate on your code, keep track of your changes, and control the exact code version your users have access to.

There are two types of deployments:

1. Head deployments, which are always synced to the current project code.
    - A head deployment is the current project code. When you create an Apps Script project, you automatically create a head deployment for that project.
    - The head deployment is always in sync with most recently saved code. For example, if you create a versioned deployment and then modify your code, the head deployment reflects those changes, while the versioned deployment remains intact.
    - Use head deployments to test code. Don't use head deployments for public use.
    - There is only ever one head deployment for each Apps Script project. To use a head deployment, you must have at least read access to the script project
1. Versioned deployments, which are connected to a specific project version.
    - A versioned deployment makes a specific version of the project code available. This lets your users continue to use a functioning version while you make changes and improvements to the code.
    - When your application is published for public consumption, always use a versioned deployment. You can have multiple active versioned deployments at one time.

Seems like deployments can be archived but not deleted

## Settings

- runtime version
- Timezone
- seem to be stored in `appscript.json` which you can enable visibility on
    > An Apps Script project manifest is a special JSON file that specifies a basic project information that Apps Script needs to run the script successfully.
    >
    > Apps Script automatically creates and updates the project manifest as you create your script project and make changes in the Apps Script editor. In most cases you never need to view or edit the manifest directly; however, in certain situations it may be beneficial or required.

## Code

- It's JS (presumably fairly modern Chrome runtime)
- Initial file is called `Code.gs`
- the `initialize()` function is special
    - my example project sets up the "on form submit" trigger in the initialize function

? what is console.log?

you can connect the code to any of the usual GCP and Google Account APIs
- can use `npm install @google/clasp -g` to write code locally and upload it

### Logging

- Logging: https://developers.google.com/apps-script/guides/logging
- https://developers.google.com/apps-script/reference/base/logger

> A basic approach to logging in Apps Script is to use the built-in execution log. To view these logs, at the top of the editor, click Execution log. When you run a function or use the debugger, the logs stream in real time.
> You can use either the Logger or console logging services in the built-in execution log.
> These logs are intended for simple checks during development and debugging, and do not persist very long.

### Runtime

> Historically, Apps Script has been powered by Mozilla's Rhino JavaScript interpreter. While Rhino provided a convenient way for Apps Script to execute developer scripts, it also tied Apps Script to a specific JavaScript version (ES5). Apps Script developers can't use more modern JavaScript syntax and features in scripts using the Rhino runtime.
> To address this concern, Apps Script is now supported by the V8 runtime that powers Chrome and Node.js. You can migrate existing scripts to V8 in order to take advantage of the modern JavaScript syntax and features.

- ES6 modules are not yet supported in V8 runtime

### Libraries

> A script that uses a library doesn't run as quickly as it would if all the code were contained within a single script project. Although libraries can make development and maintenance more convenient, use them sparingly in projects where speed is critical. Because of this issue, library use should be limited in add-ons.

> If you want one or more methods of your script to not be visible (nor usable) to your library users, you can end the name of the method with an underscore. For example, myPrivateMethod_()

- You can save code to a "Library" project
- Then other projects can look up that code using the script ID of the library - bit clunky first time

## Triggers

- identify which JS function to run in response to the trigger
- example triggers
    - when form is submitted to spreadsheet
- A project can have multiple triggers
    - but it seems like they must all come from same spreadsheet (container)


Sources

1. Spreadsheet
    - Event types
        1. On Open
        1. On Edit
        1. On Change
        1. On form submit
1. Time driven (presumably cron?)
    - various options to control how often it runs
1. Calendar
    - On calendar updated

Can get failure notifications from the trigger too

