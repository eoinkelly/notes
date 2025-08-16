# negative aspects of CSS preprocessors

mixin = advanced copy & paste - the properties are repeated on each copy+paste
=> code bloat mixins should never be used without _needing_ to pass in
arguments. prefer to use extend instead

extend sets teh properties to multiple selectors + avoids code dup0lication -
performance problems if you have many selectors - the classes you create that
will only be used via .extend are still included in the CSS even if they are
never used. can cause name collisions, bigger file size

extend and mixins tightly couple the "base classes" ot where they are used. any
change to the base class will affect **all** classes that @include it. this has
a ripple effect as any change in the base will have wide-reaching and possibly
unintended chagnes in other modules. especially as base classes may be defined
in a different area of the source .css file
