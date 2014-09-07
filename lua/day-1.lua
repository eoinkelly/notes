
-- how do i create an object - it it just a table

-- create a table ?
Monster {
  name     = 'knight',
  treasure = { -1000, 200 },
  speed    = function() return 10 * damage_to_player() end
}

-- all variables are global unless you prefix them with the `local` prefix
local cube_size = 30

Vehicle {
  name = "mine cart",
  width =  cube_size,
  height = cube_size, -- it is ok with trailing comments
}
-- comm
