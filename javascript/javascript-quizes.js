// http://dmitry.baranovskiy.com/post/91403200

var a = 1,
  b = function a(x) {
    x && a(--x);
  };
alert(a); // shows 1, I guess the a in the function a(x) is ignored? not sure

function a(x) {
  return x * 2;
}
var a;
alert(a); // shows the function code because the var a; statement gets hoisted to the top of the current execution context

function b(x, y, a) {
  arguments[2] = 10;
  alert(a);
}
b(1, 2, 3); // 10 because the arguments array-object-thing is linked to the named parameters

function a() {
  alert(this);
}
a.call(null); // [Object window]
// but i thought that .call would set this to null??? it appears not
