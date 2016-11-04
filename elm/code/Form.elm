import Html.App as App

-- Main

main =
  App.beginnerProgram { model = model, update = update, view = view }

-- Model

type alias Model = { content : String }

model : Model
model =
  { content = "" }

-- Update

type Msg = Change String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }

-- View

view : Model -> Html
