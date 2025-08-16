// https://youmightnotneed.com/lodash

export const isObject = (a: unknown): a is object => a instanceof Object;

export const isArray = (a: unknown): a is Array<unknown> => Array.isArray(a);

export const isInteger = (a: unknown): a is number => Number.isInteger(3);

export const isNumber = (a: unknown): a is number => typeof a === 'number';

export const isBigInt = (a: unknown): a is bigint => typeof a === 'bigint';

// Numbers in JavaScript are all floating point
export const isFloat = (a: unknown): a is number => isNumber(a);

export const isNull = (val: unknown): val is null => val === null;

export const isUndefined = (val: unknown): val is undefined =>
  val === undefined;

// is not null or undefined, an alternative to checking for falsiness
export const isPresent = (val: unknown) => !isMissing(val);

// is undefined or null
export const isMissing = (val: unknown) => val === undefined || val === null;

export const isBoolean = (arg: unknown): arg is boolean => arg === !!arg;

export const isDate = (d: unknown): d is Date => d instanceof Date; // && !Number.isNaN(d) // todo: why is this check here?

export const isError = (err: unknown): err is Error => err instanceof Error;

export const isFunction = (val: unknown): val is Function =>
  typeof val === 'function';

export const isRegExp = (obj: unknown): obj is RegExp => obj instanceof RegExp;

export const isString = (a: unknown): a is String => typeof a === 'string';

export const isSymbol = (symbolMaybe: unknown): symbolMaybe is Symbol =>
  typeof symbolMaybe === 'symbol';

// isNaN() exists as a global function but it does surprising coercion of the
// argument so you should use Number.isNaN() instead. `isNumberNaN` exists just
// so you can use the same function name pattern as other type checks.
export const isNumberNaN = (maybeNaN: unknown) => Number.isNaN(maybeNaN);

const isEqual = (a: unknown, b: unknown): boolean => {
  if (a !== b) {
    console.log('Not equal:', a, b);
    throw new Error('Not equal');
  }
  return true;
};

console.log('Starting tests');

isEqual(isArray([]), true);
isEqual(isArray(''), false);
isEqual(isArray(undefined), false);
isEqual(isArray(NaN), false);
isEqual(isArray(0), false);

isEqual(isPresent(0), true);
isEqual(isPresent(NaN), true);
isEqual(isPresent(null), false);
isEqual(isPresent(undefined), false);
isEqual(isPresent(''), true);

isEqual(isDate(new Date()), true);
isEqual(isDate(0), false);
isEqual(isDate(''), false);
isEqual(isDate(NaN), false);
isEqual(isDate('2011-03-04'), false);

console.log('Done');
