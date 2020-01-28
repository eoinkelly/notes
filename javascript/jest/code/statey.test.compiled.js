"use strict";

var _statey = require("./statey");

test("test stateful module transipling", () => {
  console.log("Counter:", _statey.counter);
  (0, _statey.inc)();
  console.log("Counter:", _statey.counter);
  expect(1).toEqual(1);
});
