// Asynchronous Loading
// ====================
// This method has good cross-browser support unlike
//  <script async="async" src=""></script>
// which only works in newer browsers

// Pattern 1:


// Optional: Create a global variable that will hold any state and functionality that is unique to this page
// Google Webfonts do this.
// - We are polluting the global namespace
PerPageGlobalConfig = {
  foo: 'bar'
}

// Create a script DOM node and insert it before any other <script> tags on the page
(function() {
  var node = document.createElement('script');
      node.type = 'text/javascript';
      node.async = true;
      // node.src = ('https:' == document.location.protocol ? 'https' : 'http') + '//www.example.com/example.js';
      node.src = 'example.js';

  var s = document.getElementsByTagName('script')[0]; // Find the first script node on the page
  s.parentNode.insertBefore(node, s); // Insert our new script node before it, ensuring it is run before any other JS on the page
  // Since this IIFE is running on the page, we know there will be at least one piece of JS in the page
})();