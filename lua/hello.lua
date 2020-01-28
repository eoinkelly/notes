
-- xx = "hello world"
-- print(type(xx))
-- print("hello world")
-- print(math.ceil(123.56))

-- msg = io.read()
-- print("the message is" .. msg)

local foo = 1

do
  -- local foo = 44
  local foo_2 = foo + 2
  print("foo_2: " .. foo_2) -- => 3
end

print("foo: " .. foo) -- => 1
-- print(foo_2) -- => nil and luacheck will warn about creating an undefined variable
--

function blah()
  print("hi there from blah")
  return 123, "hello"
end

code, msg = blah()
print(code)
print(msg)


code2  = blah()
-- the second return value is discarded
print(code2)

for x = 0, 10, 1 do
  -- new chunk in here
  print(x)
end

local things_table = { a = "moon", b = "sun", c = "mars" }

for i, planet in pairs(things_table) do
  print(i)
  print(planet)
end

print("************************************")
local things_array = { "moon", "sun", "mars", other = "pluto" }

-- ipairs will ignore any non numeric key in a table
for i, planet in ipairs(things_array) do
  print(i)
  print(planet)
end
-- outputs:
-- 1
-- moon
-- 2
-- sun
-- 3
-- mars

print("************************************")

-- pairs will return **all** keys, numeric or otherwise
for i, planet in pairs(things_array) do
  print(i)
  print(planet)
end
-- outputs
-- 1
-- moon
-- 2
-- sun
-- 3
-- mars
-- other
-- pluto
print("************************************")


-- this snippet from https://www.luafaq.org/#T1.10 works ok for simple tables
-- but can't handle circular references
function dump(o)
    if type(o) == 'table' then
        local s = '{ '
        for k,v in pairs(o) do
                if type(k) ~= 'number' then k = '"'..k..'"' end
                s = s .. '['..k..'] = ' .. dump(v) .. ','
        end
        return s .. '} '
    else
        return tostring(o)
    end
end

print(dump(things_table))
print("************************************")

print(type(import))

print("************************************")
print("require style 1")

print(package.path)
-- /usr/local/share/lua/5.3/?.lua;/usr/local/share/lua/5.3/?/init.lua;/usr/local/lib/lua/5.3/?.lua;/usr/local/lib/lua/5.3/?/init.lua;./?.lua;./?/init.lua

-- local uts = require("uselessThings")
-- uts.sayHi()

print("************************************")
print("require style 2")

require("uselessThings")
uselessThings.sayHi()
print(someOtherRandomThing)
print(aa)

local Car = require("Car")

-- local mazda = Car.new(Car, nil)
local mazda = Car:new() -- exactly same as above

-- mazda.drive(mazda)
mazda:drive() -- exactly same as line above

local atv = Car:new({ wheels = 8 }) -- exactly same as above
atv:drive()
