
They like the _data flow model_ from React
    * well written ember apps already use
* 2 will have a virtual DOM
* React virtual DOM allowed them to simplify the programming model of _component based applications_.

when you enter a route:
1. it builds a controller
2. associates a model with it
3. hands the controller to the view for rendering

currently ObjectController and ArrayController magically proxy model properties - this will stop - you will need to use `model.propName`

top level components will **not** perisist across naivation
    => persistent state should be stored in route objects and passed into the "routeable component" when it is instantiated
route objects will have an `attrs` hook which can return other asy,c data that the component might need

currently ember has routable controllers which are persistent, in future it will have routeable components which do not persist

routeable componetns should be stored on filesystem in "pod" format
