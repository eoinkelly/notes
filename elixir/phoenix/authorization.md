# Authorization

Questions

- Do it in the web layer or within the context?

Options

1. roll your own as a Plug (either function or module)
2. https://github.com/jfrolich/authorize
3. https://github.com/boydm/policy_wonk
4. https://github.com/schrockwell/bodyguard
5. https://github.com/jarednorman/canada
6. https://github.com/cpjk/canary

## Rolling your own

1. create a Plug which finds the "current user" and checks if it can perform the
   requested action. If not it halts processing the conn

q: how do I do this without having an `/authenticated` prefix?
