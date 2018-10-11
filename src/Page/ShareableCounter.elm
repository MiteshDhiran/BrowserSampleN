module Page.ShareableCounter exposing (Config(..), State, config, initialState, isCounterIDEqual, view)

import Html exposing (Html)
import Html.Events
import Json.Decode as Json


{-| Tracks the counter value.
-}
type State
    = State Int Int


initialState : Int -> Int -> State
initialState counterId initialValue =
    State counterId initialValue


isCounterIDEqual : State -> State -> Bool
isCounterIDEqual (State a b) (State c d) =
    a == c


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
view ((Config { toMsg }) as conf) (State counterId counterVal) =
    Html.label [ onClick counterId counterVal toMsg ] [ Html.text ("Counter ID:" ++ Debug.toString counterId ++ "COUNTER VALUE:" ++ Debug.toString counterVal) ]


onClick : Int -> Int -> (State -> msg) -> Html.Attribute msg
onClick counterId counterVal toMsg =
    Html.Events.on "click" <|
        Json.map toMsg <|
            Json.map2 State (Json.succeed counterId) (Json.succeed (counterVal + 1))



{-
   toCounterHtml : State -> (State -> msg) -> ( Html msg)
   toCounterHtml (State counterVal) toMsg =
     Html.label [ onClick counterVal] [ Html.text (Debug.toString (counterVal)) ]
-}
