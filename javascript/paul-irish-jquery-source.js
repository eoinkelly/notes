/*
Self executing anonymous function
This immediately executing function is a popular pattern because:
*   By wrapping our code in a function, we can create variables without putting them
    in the global scope.
*   Safe 'undefined' value:
    We call our function with 2 args so the 3rd is garuanteed to be undefined. This
    protects us from some other code on the page messing with the value of 'undefined'
    e.g. undefined = true; would be a Bad Thing(TM)
*   Because we are passing 'window' and 'document' as parameters, we get a slight
    benefit when we minify as the minifier can safely replace them with shorter
    strings e.g. aa, bb.
*   The is a very slight performance win by the VM not having to traverse up to
    global scope to get values for 'window' and 'document'.
*   More info: http://www.youtube.com/watch?v=i_qE1iAmjFg
*/

(function(window,document,undefined){

})(this,this.document);
// })(this,document); // also works

setInterval(function(){
    doStuff();
}, 100); // Run the function every 100mS

/*
This pattern is superior because it works ok if doStuff() takes longer thant 100ms
*   It leaves a gap of 100mS between each call to doStuff() rather than calling
    doStuff() every 100mS
*/

(function looper(){
    doStuff();
    setTimeout(looper,100);
})();


