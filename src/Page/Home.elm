module Page.Home exposing (..)

import Session exposing (..)
import Html as H exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Style exposing(..)
import Style.Color as StyleColor exposing (..)
import Style.Font as Font
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Input
import Element.Events

type alias Model =
    {
        session : Session
      , content : String
    }

type Msg
    = None
    | GoToLogin
    | GotSession Session

init : Session -> ( Model, Cmd Msg )
init session =
    (
        {   session = session
          , content = case session of
                        LoggedIn k viewer -> "viewer"
                        Guest k -> "Guest User"
        }
      , Cmd.none
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None -> (model, Cmd.none)
        GoToLogin -> (model, Cmd.none)
        GotSession session ->
                    ( { model | session = session }, Cmd.none )

view : Model -> { title : String, content : Html Msg }
view model =
        {
             title = "Home Page"
           , content = H.div [] [
                                      H.text "Home content goes here"
                                    , H.a [ Html.Events.onClick GoToLogin
                                           ,Html.Attributes.href "#login"]
                                           [H.text "Go To Login"]
                                 ]
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)
