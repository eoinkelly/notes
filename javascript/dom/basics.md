
https://ponyfoo.com/articles/uncovering-the-native-dom-api


XMLHttpRequest
fetch api


// W3C version
element.addEventListener
element.removeEventListener

// IE version
element.attachEvent
element.detachEvent

// finding stuff

document.querySelector
document.querySelectorAll
document.getElementById
document.getElementsByTagName
document.getElementsByClassName


document.createDocumentFragement
document.createElement
appendChild

http://ejohn.org/blog/dom-documentfragments/

Animations and movement
requestAnimationFrame

DOM is API for HTML, XML and SVG

Event handlers consume resources

QUESTION: how does the C++ DOM and JS dom interact?

DOM uses DOMString which maps exactly to a JS String (both are UTF-16)

Things that return more than one object return a`NodeList` which is like an array but it will automatically update as the DOM nodes it refers to change
    i.e. it is a mutable array which may change at any time
