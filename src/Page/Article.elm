module Page.Article exposing (Model, Msg(..), init, subscriptions, update, view)

import Html exposing (..)
import Session exposing (Session)


type alias Model =
    { session : Session
    }


type Msg
    = ArticleLoaded
    | GotSession Session


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Article"
    , content = Html.text "Article content goes here"
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ArticleLoaded ->
            ( model, Cmd.none )

        GotSession session ->
            ( { model | session = session }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)
