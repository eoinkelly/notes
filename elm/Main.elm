-- import Mouse
--
-- main = lift asText Mouse.position

import Html exposing (text)

main =
  let
      x = 22
      point = { x = 3, y = 44 }
      _ = useTheConsole
      _ = endItAll
  in
      if 3 < 20 then
        text "Hello small"
      else
        text "Hello big"


-- functions must return a value so we have to use this extra stuff
-- I am approximating a "null" return value by having a function which returns Nothing (a constructor of the Maybe a type
useTheConsole : Maybe a
useTheConsole =
  let
      -- * this is how you console.log
      -- * you must give Debug.log two args or it will not print anything
      _ = Debug.log "prefixed message" 0
      _ = Debug.log "other prefixed message"  0
  in
      Nothing

endItAll : Maybe a
endItAll =
  let
      -- Debug.crash becomes console.error
      _ = Debug.crash "bye bye"
  in
      Nothing


