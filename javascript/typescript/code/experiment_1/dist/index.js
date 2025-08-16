'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        Object.defineProperty(o, k2, {
          enumerable: true,
          get: function () {
            return m[k];
          }
        });
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
Object.defineProperty(exports, '__esModule', { value: true });
const carHelpers = __importStar(require('./option1'));
const option1_1 = require('./option1');
let myCar = carHelpers.create();
carHelpers.drive(myCar);
let myCar2 = option1_1.create();
option1_1.drive(myCar2);
class Thing {
  constructor(a, b) {
    Object.defineProperty(this, 'a', {
      enumerable: true,
      configurable: true,
      writable: true,
      value: void 0
    });
    Object.defineProperty(this, 'b', {
      enumerable: true,
      configurable: true,
      writable: true,
      value: void 0
    });
    console.log(`Running Thing constructor`);
    this.a = a;
    this.b = b;
    console.log(this.privateInstanceMethod());
  }
  static publicMethod() {
    return 'publicMethod';
  }
  static privateMethod() {
    return 'privateMethod';
  }
  publicInstanceMethod() {
    return 'publicInstanceMethod';
  }
  privateInstanceMethod() {
    return 'privateInstanceMethod';
  }
}
Object.defineProperty(Thing, 'publicByDefault', {
  enumerable: true,
  configurable: true,
  writable: true,
  value: 'publicByDefault'
});
Object.defineProperty(Thing, 'publicByExplicit', {
  enumerable: true,
  configurable: true,
  writable: true,
  value: 'publicByExplicit'
});
Object.defineProperty(Thing, 'privateByExplicit', {
  enumerable: true,
  configurable: true,
  writable: true,
  value: 'privateByExplicit'
});
console.log(Thing.publicByDefault);
console.log(Thing.publicByExplicit);
let t = new Thing(22, 33);
console.log(t);
console.log(t.publicInstanceMethod());
const mimic = __importStar(require('./mimicClass'));
console.log(mimic.publicStaticIsh());
let mm = mimic.construct(55, 33);
mimic.greet(mm);
console.log(mimic.someDict);
mimic.someDict['hello'] = 'there';
console.log(mimic.someDict);
const mimicClass_1 = require('./mimicClass');
console.log('alias:', mimicClass_1.someDict);
