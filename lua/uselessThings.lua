-- this style of module is more disiplined - it only returns one thing, doesn't pollute the global namespace but must be assigned to a variable when required
-- local uselessThings = {}
--
-- uselessThings.name = "Miles O'Brien"
-- uselessThings.sayHi = function()
--   print("hi")
-- end
--
-- return uselessThings


-- use this style of module to automatically create
-- modules implemente din this style are used by require() with assigning them to anything - we are adding 'uselessThings' as a global variablej
-- require("uselessThings")
uselessThings = {}

someOtherRandomThing = "I AM A GLOBAL POLLUTION!" --non local variables are created in global scope and available anytime after this file is rquire'd
local aa = 123 -- local variables don't leak out of this file
uselessThings.name = "Miles O'Brien"
uselessThings.sayHi = function()
  print("hi")
end

-- return uselessThings
