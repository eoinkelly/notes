# CSS Hacks

http://paulirish.com/2009/browser-specific-css-hacks/ .box { background: #00f;
/_ all browsers _/ _background: #f00; /_ IE 7 and below _/ \_background: #f60;
/_ IE 6 and below _/ background: #fff\n; /_ IE 8 and below\*/ }

Hacks are better than browser specific classes as they dont' change specificity

Can hack declarations be created with JS? I don't think so.

Hacks in a separate file:

- easy to find and remove when supporting those browsers is no longer required
- can load them separately using conditional comments. could be a bonus if there
  are a lot of hacks

* hack declarations are out of context - makes it harder to edit the CSS until
  that fateful day. This is a biggie

I will include hacks in the main CSS and mark them with a comment

http://en.wikipedia.org/wiki/CSS_filter
