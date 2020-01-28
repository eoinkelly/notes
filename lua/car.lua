
-- create the prototype object with some default attribute values
local Car = {
  color = nil,
  wheels = 4
}

Car.new = function(self, newObject)
  newObject = newObject or {}

  -- make Car the metaobject of our new object
  setmetatable(newObject, self)

  -- since Car is now a metaobject too we can set __index to point at Car. This
  -- will mean that attributes missing from newObject will be looked up in Car
  self.__index = self

  return newObject
end

Car.drive = function(self)
  print("Driving on wheels: " .. self.wheels)
end

return Car
