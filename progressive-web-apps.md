# PWAs

## Explainer for non-developers (2018-11-18 edition)

* A Progressive web app (PWA for short) is a marketing term
* The term describes a group of features introduced into web browsers over the last few years to allow applications built with HTML, CSS & Javascript to have features which were, before that, only available to native mobile apps
* It is important to note that the comparison is against native **mobile** apps.
* Any HTML/CSS/JS app includes some or all of these features is termed a PWA. Note an app does not have to include **all** the features.

The features are

* Be able to
*
*

PWA does not mean "it's a native app but we get to do it in HTML/CSS/JS". There _may_ be a future where this is true but it seems extremely unlikely that Apple would abandon their own app store ecossytem in favour of PWAs so


There are still (in 2018) significant caveats to building a PWA

Whether you think a PWA is a good technical solution depends largely on what you are comparing it to.

If you compare a PWA against the
A PWA does not mean

There is still a significant feature gap between PWAs and native apps. It is true that apps built in the PWA style are the closest web and native have ever been, that does not mean they is parity.

A PWA is "just" a web app. It does not use any "build native app with web technologies" frameworks like Cordova or React-native.

Broadly speaking, the features of web apps are best supported on modern Android devices. Google is largely responsible for the marketing hype behind "progressive web apps". Apple are implementing the features required but with much less enthusasism.



https://tomayac.github.io/pwa-feature-detector/

## Background

* Service worker
    * Recommended tool: workbox (an evolution of sw-precache)
        * https://developers.google.com/web/tools/workbox/
