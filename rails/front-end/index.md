# Front-ends with Rails

## Overview of Approaches

* Option: Render **all** HTML on server (Server sends HTML)
    * Old: Rails + UJS + Turbolinks
    * New: Rails + Turbo + Stimulus (if required) + React (if required)
* Render **most** HTML on server (Server sends JSON, JS or HTML)
    * Old: Rails + UJS + Turbolinks
    * New: Rails + Turbo + Stimulus (if required) + React (if required)
* Render **all** HTML on client (server sends JSON only)
    * Rails + React

? view components on server side


## Overview of Tools

* Basecamp stuff
    1. Rails UJS
    2. Turbolinks
    3. Turbo (part of Hotwire)
    4. Stimulus (part of Hotwire)
* Other open source stuff
    1. StimulusReflex
    1. React etc.

##  Rails UJS

https://guides.rubyonrails.org/working_with_javascript_in_rails.html

* A few conventions for submitting links, buttons, forms via AJAX
* Jobs
    * let you add data attrs to things that will popup a browser confirm dialog
    * let you disable a form button during submitting
    * can submit forms via AJAX (by setting `data-remote=true` attr) and let you add listeners to the `ajax:success` ans `ajax:failure` events to get the response
    * the response can be (depending on the controller and how you hit the route)
        1. HTML rendered on the server that the JS should insert in the page
        2. JSON that you process manually in JS
        3. a snippet of JS that UJS will insert into a script element which causes browser to evaluate it
* you interact with it by
    * adding `data-` attributes to elements e.g. `data-remote=true` which causes form submissions to be sent via AJAX
    * and also add listeneres to event names defined by UJS so that you can get the replies back.
    * The server action which returns the reply can send back:
        1. HTML which your listener can insert into the DOM
        1. JSON which your listener can process
        1. Javascript which your listener can evaluate
            * done via `myaction.js.erb` and a `format.js` in the controller
            * UJS will automatically create a `<script>` element for you in the DOM which will cause it to turn
    * Rails helpers have built-in support for adding them e.g. `link_to`
* Hasn't needed jQuery since Rails 5.1
* THe "unobtrusive" bit is that you are not inlining your JS into `onclick=` etc. Old school :-)


## Turbolinks

* Attaches a click handler to `<a>` tags on the page
* Works for links only - catches the click, sends the normal request to the server, replaces the <body> with the response and uses browser `pushState` to keep the back button working
    * = Same traffic goes over network as with a normal page load
    * ++ the user doesn't experience the flash of a full page replace
* Has been superseded by Turbo

## Turbo

* In beta as of Jun 2021
* Created by Basecamp
* All about sending HTML over the wire
* Made up of
    1. Turbo drive
        * replaces link and form submissions
    2. Turbo frames
        * decompose pages into independent contexts which
            1. scope navigation
            2. can be lazily loaded
    3. Turbo streams
        * deliver page changes over Websockets, SSE or in response to form submissions
    4. Turbo native
        * for use by native apps
* A typescript library
* the successor for turbolinks (but it does more. turbolinks is really just "turbo drive")
* Turbolinks just dealt with links. Turbo does form submissions and responses too
* Turbo streams
    * does require websockets
    * delivers page updating HTML asyncronously over the websocket

* `gem 'turbo-rails'` to install

Compatibility with UJS

* Rails 6.1 turns UJS submitting via XHR off for forms
* you can force this in earlier versions with `config.action_view.form_with_generates_remote_forms = false`

* you can still use `data-confirm` and `data-disable-with`

## Stimulus

## Hotwire = Turbo + Stimulus

Marketing term.

## StimulusReflex

* Kind of LiveView for Rails
* tries to help you avoid writing any JS

## React
