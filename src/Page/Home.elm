module Page.Home exposing (Model, Msg(..), init, subscriptions, update, view)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events
import Element.Input
import Html as H exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import PStyle
import Session exposing (..)



{- import Style exposing (..)
   import Style.Color as StyleColor exposing (..)
   import Style.Font as Font
-}


type alias Model =
    { session : Session
    , content : String
    }


type Msg
    = None
    | GoToLogin
    | GotSession Session


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , content =
            case session of
                LoggedIn k viewer ->
                    "viewer"

                Guest k ->
                    "Guest User"
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        GoToLogin ->
            ( model, Cmd.none )

        GotSession session ->
            ( { model | session = session }, Cmd.none )


view : Model -> { title : String, content : Html Msg }
view model =
    let
        isLoggedInUser =
            case model.session of
                LoggedIn key viewer ->
                    True

                _ ->
                    False

        loggedUserName =
            Session.getUserName model.session
    in
    { title = "Home Page"
    , content =
        Element.layout PStyle.stylesheet <|
            column PStyle.None
                []
                [ el PStyle.None [] (Element.text "Home content goes here")
                , Element.when isLoggedInUser (el PStyle.None [] (Element.text loggedUserName))
                , Element.when (isLoggedInUser == False) (el PStyle.NavOption [] (link "#login" (el PStyle.None [] (Element.text "Go To Login"))))
                , Element.when (isLoggedInUser == True) (el PStyle.NavOption [] (link "#logout" (el PStyle.None [] (Element.text "Log Out"))))
                ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)
