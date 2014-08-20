Stack Vs. Heap
==============

* http://stackoverflow.com/questions/79923/what-and-where-are-the-stack-and-heap

* Each thread gets a stack
* Stack is LIFO
* The size of the stack is set when the thread is created
* Used for function calls and parameters and return pointers


Heap
* In C stuff is put on the heap via new and malloc
* Can be very large
* Slower to allocate than the stack
* Used for objects

Javascript Stack Size
---------------------

* Varies by browser
* Can test with
var i=0;
function inc() {
    i++;
    inc();
}
inc();

Some stats from: http://stackoverflow.com/questions/7826992/browser-javascript-stack-size-limit
Internet Explorer
IE6: 1130
IE7: 2553
IE8: 1475
IE9: 20678
IE10: 20677
Mozilla Firefox
3.6: 3000
4.0: 9015
5.0: 9015
6.0: 9015
7.0: 65533
8b3: 63485
Google Chrome
14 (stable): 26177
15 (beta): 26168
16 (canary): 26166
Safari
4: 52426
5: 65534
Opera
10.10: 9999
10.62: 32631
11: 32631

I got 25018 for chrome 23 on my laptop