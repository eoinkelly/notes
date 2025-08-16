'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.someDict =
  exports.SOME_VAL =
  exports.generateGreeting =
  exports.greet =
  exports.construct =
  exports.privateStaticIsh =
  exports.publicStaticIsh =
    void 0;
const publicStaticIsh = () => {
  console.log('publicStaticIsh');
};
exports.publicStaticIsh = publicStaticIsh;
const privateStaticIsh = () => {
  console.log('privateStaticIsh');
};
exports.privateStaticIsh = privateStaticIsh;
const construct = (a, b) => {
  console.log(a, b);
  return {
    a: a,
    b: b
  };
};
exports.construct = construct;
const greet = self => {
  console.log(exports.generateGreeting(self), self.a, self.b);
};
exports.greet = greet;
const generateGreeting = self => {
  return 'Greetings from: ';
};
exports.generateGreeting = generateGreeting;
exports.SOME_VAL = 12235;
exports.someDict = {};
