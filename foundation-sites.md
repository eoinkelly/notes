# Foundation 6

Has two main sections

1. sites - stuff for responsive websites
2. emails - stuff for responsive emails

There is a separate CLI tool called foundation-cli

* https://github.com/zurb/foundation-cli
* downloads & installs blank templates for you
* can also run a build watcher

THe sass version:

* when installed from npm, also installs
    * jquery
    * what-input

### what-input

* changes the value of `data-whatinput` on `<html>` input to indicate what kind of input is was last in use e.g. mouse, keyboard, touch
* changes teh value of `data-whatintent` to tell you when touch is being used as a mouse etc.
* it does it by attaching an event listener to `<html>` and listening for the many kinds of events e.g mouseup, mousedown etc.

* doesn't count typing in form `<input>` or `<textarea>` as a new kind of input

```
when the page first loads

<html lang="en" data-whatinput="initial" data-whatintent="initial" class=" whatinput-types-initial">

after moving around

<html lang="en" data-whatinput="initial" data-whatintent="mouse" class=" whatinput-types-initial">

and then clicking

<html lang="en" data-whatinput="mouse" data-whatintent="mouse" class=" whatinput-types-initial whatinput-types-mouse">

and then hitting tab on keyboard

<html lang="en" data-whatinput="keyboard" data-whatintent="mouse" class=" whatinput-types-initial whatinput-types-mouse whatinput-types-keyboard">
```


Using the SCSS version

* settings file must be imported _before_ the main foundation file
    * there is no automatic update of settings file between foundation releases
    * you have to many copy the starter settings file from foundation and import it into your project (the ember generator does this in the case of ember-cli-foundation-6-sass addon)
* foundation is split into "components"
    * each component has a core set of SCSS variables which modify its styling

There are two main ways to use foundation

```
// import everything
@import 'settings';
@import 'foundation';
@include foundation-everything
```

vs

```
// import each component separately
@import 'settings';
@import 'foundation';

@include foundation-global-styles;
@include foundation-grid;
@include foundation-flex-grid;
@include foundation-flex-classes;
@include foundation-typography;
@include foundation-forms;
@include foundation-button;
@include foundation-accordion;
@include foundation-accordion-menu;
@include foundation-badge;
@include foundation-breadcrumbs;
@include foundation-button-group;
@include foundation-callout;
@include foundation-card;
@include foundation-close-button;
@include foundation-menu;
@include foundation-menu-icon;
@include foundation-drilldown-menu;
@include foundation-dropdown;
@include foundation-dropdown-menu;
@include foundation-responsive-embed;
@include foundation-label;
@include foundation-media-object;
@include foundation-off-canvas;
@include foundation-orbit;
@include foundation-pagination;
@include foundation-progress-bar;
@include foundation-slider;
@include foundation-sticky;
@include foundation-reveal;
@include foundation-switch;
@include foundation-table;
@include foundation-tabs;
@include foundation-thumbnail;
@include foundation-title-bar;
@include foundation-tooltip;
@include foundation-top-bar;
@include foundation-visibility-classes;
@include foundation-float-classes;
```

File lengths of the foundation zip download

essential/foundation.css ~ 2500 lines (approx 64 kb)
essential/foundation.js ~ 1960 lines (approx 64 kb)

complete/foundation.css ~ 4380 lines (approx 110 kb)
complete/foundation.js ~ 9977 lines (approx 326 kb)
