# 52 end of mocking



# mocking smells 4

All of these smells cause problems when you refactor your code not when you first write it and the tests

1. Don't stub methods on the object under test
    * Put the stubs/mocks on methods of the _collaborator objects_ not the
      object you are testing - test that the method was received not that the
      message was sent!!!
2. "Only mock types you own"
    * Do not mock system calls and system IO
    * the mock duplicates the implementation of the thing being mocked
    * when you get to the edge of your system, stop mocking and do real tests
3. Don't use doubles in place of value objects
    * if the collobarator is a simple value object, don't use a test double,
      use an instance of the real collobarator object
4. Identify the "inputs" of the method and the "outputs" of the method.
    * inputs should be doubled with a stub not a mock - otherwise we couple the
      test to the implementation more than we need to
    * outputs should be doubled with a mock

Putting a mock on a message the objec tunder test sends itself is an anti-pattern
> The purpose of the test it to show how the object interacts with the objects around it. showing that it interacts with itself tells us nothing of interest
=> all expections should be set on collaborator objects not the object-under-test

An expection with a return value is a smell
anything more than one message expection per test is a smell

"Adapter classes" are those classes that connect our code to the outside world - these are poor choices for mocking - they should be tested in an integration style

If you mock out the bit of the large ruby FileIO or SystemIO API that your
object currently uses you _are_ coupling your test to the specific
implementation of your object

@avdi answer to "are these tests not slow" is that if the object under test is interacting with a thing we don't control (external API, filesystem etc.) then it should be very thin and just only that - it should be injected into all business objects
The answers to a lot of these smells seems to be inject the dependencies

THere is a paper on this by freeman et al

Reasons
1. Ruby FileIO is stable but there are many ways to achieve the same thing. If your mock a particular way then you will have to update each mock every time you change how you do the fileIO
