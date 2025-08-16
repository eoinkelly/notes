# Vitest

Mocking modules

In your test file you can hook into modules loaded elsewhere in your code you
can obeserve inputs and outputs and replace implementations

## Automatic mocking

If you mocke a moculde and don't provide any implementations, vitest will use
default implementations for every export

1. All exported arrays are emptied
2. Exported primitive values and collections stay the same
3. Exported objects are deeply cloned
4. Exported class instances and their prototypes are deeply cloned

Why is this useful?

If module A exports methods X and Y. If X calls Y, it will always call the real
Y - it will never call your mocked Y. Code that calls Y from outside will see
your mock
