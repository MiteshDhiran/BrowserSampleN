module Page.ShareableCounter exposing (Config(..), State, config, initialState, view)

import Html exposing (Html)
import Html.Events
import Json.Decode as Json


{-| Tracks the counter value.
-}
type State
    = State Int


initialState : Int -> State
initialState initialValue =
    State initialValue


{-| Configuration for your counter module, describing methods.

**Note:** Your `Config` should _never_ be held in your model.
It should only appear in `view` code.

-}
type Config msg
    = Config
        { toMsg : State -> msg
        }



{- Constructor for config -}


config : { toMsg : State -> msg } -> Config msg
config { toMsg } =
    Config
        { toMsg = toMsg
        }


view : Config msg -> State -> Html msg
view ((Config { toMsg }) as conf) (State counterVal) =
    Html.label [ onClick counterVal toMsg ] [ Html.text ("COUNTER VALUE:" ++ Debug.toString counterVal) ]


onClick : Int -> (State -> msg) -> Html.Attribute msg
onClick counterVal toMsg =
    Html.Events.on "click" <|
        Json.map toMsg <|
            Json.map State (Json.succeed (counterVal + 1))



{-
   toCounterHtml : State -> (State -> msg) -> ( Html msg)
   toCounterHtml (State counterVal) toMsg =
     Html.label [ onClick counterVal] [ Html.text (Debug.toString (counterVal)) ]
-}
