
Problems with clearfix
----------------------
comment on http://www.stubbornella.org/content/2009/07/23/overflow-a-secret-benefit/
clearfix method is – imho – the worse method ever. I believe most people adopted it because it was “cool”, but not because it was “smart”. The fact that it does not create a new block-formatting context in non-IE browsers, but does trigger hasLayout in IE creates *2* very different constructs.
These are so different that you deal with collapsing margin in one browser but not in the other, background painting issues, lateral margin and padding issues, etc. (note to self: write an article about this because there are way too many authors getting into CSS hell because of this).

Aside: I don’t think using a class like “group” is any better. IMO, it’s worse because it is vague and obfuscates the very specific purpose of the class. Just because some content is grouped doesn’t mean that there are any floats that need to be contained. And very common components (like floated-image + text) that need require float containment could only be tenuously described as “groups”.

Cons of overflow: hidden;
    Another example is box-shadow gets clipped.
    It will clip off elements that flow outside the container, absolute positioned elements for example.