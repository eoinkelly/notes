import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)

main =
  -- call beginnerProgram passing it a record
  -- the values in the record are functions
  App.beginnerProgram { model = model, view = view, update = update }


-- Model

type alias Model = Int

model : Model
model =
  0

-- Update

type Msg = Reset | Increment | Decrement | Change String

-- Reset is a function which takes 0 args and returns a Msg
-- Change is a function which takes a String and returns a Msg
-- ?? functions that start with uppercase are type constructors

update : Msg -> Model -> Model
update msg model =
  case msg of
    Reset ->
      0
    Increment ->
      model + 1
    Decrement ->
      model - 1

-- View

view : Model -> Html Msg
view model =
  div []
    [
      button [ onClick Decrement ] [ text "-" ]
      , div [] [ text (toString model) ]
      , button [ onClick Increment ] [ text "+" ]
      , button [ onClick Reset ] [ text "reset" ]
    ]

