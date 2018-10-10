module Page.Counter exposing (Model, Msg(..), init, update, view)

import Html exposing (Html)
import Html.Attributes
import Html.Events


type alias Model =
    { counter : Int
    }


type Msg
    = Increment
    | Decrement


init : ( Model, Cmd Msg )
init =
    ( { counter = 1 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter + 1 }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.text (Debug.toString model.counter)
        , Html.button [ Html.Events.onClick Increment ] [ Html.text "Increment" ]
        , Html.button [ Html.Events.onClick Decrement ] [ Html.text "Decrement" ]
        ]
