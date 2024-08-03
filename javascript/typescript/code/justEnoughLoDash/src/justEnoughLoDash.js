"use strict";
// https://youmightnotneed.com/lodash
Object.defineProperty(exports, "__esModule", { value: true });
exports.isNumberNaN = exports.isSymbol = exports.isString = exports.isRegExp = exports.isFunction = exports.isError = exports.isDate = exports.isBoolean = exports.isMissing = exports.isPresent = exports.isUndefined = exports.isNull = exports.isFloat = exports.isBigInt = exports.isNumber = exports.isInteger = exports.isArray = exports.isObject = void 0;
var isObject = function (a) { return a instanceof Object; };
exports.isObject = isObject;
var isArray = function (a) { return Array.isArray(a); };
exports.isArray = isArray;
var isInteger = function (a) { return Number.isInteger(3); };
exports.isInteger = isInteger;
var isNumber = function (a) { return typeof a === 'number'; };
exports.isNumber = isNumber;
var isBigInt = function (a) { return typeof a === 'bigint'; };
exports.isBigInt = isBigInt;
// Numbers in JavaScript are all floating point
var isFloat = function (a) { return (0, exports.isNumber)(a); };
exports.isFloat = isFloat;
var isNull = function (val) { return val === null; };
exports.isNull = isNull;
var isUndefined = function (val) { return val === undefined; };
exports.isUndefined = isUndefined;
// is not null or undefined, an alternative to checking for falsiness
var isPresent = function (val) { return !(0, exports.isMissing)(val); };
exports.isPresent = isPresent;
// is undefined or null
var isMissing = function (val) { return val === undefined || val === null; };
exports.isMissing = isMissing;
var isBoolean = function (arg) { return arg === !!arg; };
exports.isBoolean = isBoolean;
var isDate = function (d) { return d instanceof Date; }; // && !Number.isNaN(d)
exports.isDate = isDate;
var isError = function (err) { return err instanceof Error; };
exports.isError = isError;
var isFunction = function (val) { return typeof val === 'function'; };
exports.isFunction = isFunction;
var isRegExp = function (obj) { return obj instanceof RegExp; };
exports.isRegExp = isRegExp;
var isString = function (a) { return typeof a === 'string'; };
exports.isString = isString;
var isSymbol = function (symbolMaybe) { return typeof symbolMaybe === 'symbol'; };
exports.isSymbol = isSymbol;
// isNaN() exists as a global function but it does surprising coercion of the
// argument so you should use Number.isNaN() instead. `isNumberNaN` exists just
// so you can use the same function name pattern as other type checks.
var isNumberNaN = function (maybeNaN) { return Number.isNaN(maybeNaN); };
exports.isNumberNaN = isNumberNaN;
var isEqual = function (a, b) {
    if (a !== b) {
        console.log('Not equal:', a, b);
        throw new Error('Not equal');
    }
    return true;
};
console.log('Starting tests');
isEqual((0, exports.isArray)([]), true);
isEqual((0, exports.isArray)(""), false);
isEqual((0, exports.isArray)(undefined), false);
isEqual((0, exports.isArray)(NaN), false);
isEqual((0, exports.isArray)(0), false);
isEqual((0, exports.isPresent)(0), true);
isEqual((0, exports.isPresent)(NaN), true);
isEqual((0, exports.isPresent)(null), false);
isEqual((0, exports.isPresent)(undefined), false);
isEqual((0, exports.isPresent)(""), true);
isEqual((0, exports.isDate)(new Date()), true);
isEqual((0, exports.isDate)(0), false);
isEqual((0, exports.isDate)(""), false);
isEqual((0, exports.isDate)(NaN), false);
isEqual((0, exports.isDate)("2011-03-04"), false);
console.log('Done');
