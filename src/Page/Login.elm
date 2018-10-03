module Page.Login exposing (Model, Msg(..), init, update, view,subscriptions)

import Html as H exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events as HE exposing (..)
import Session exposing (Session)
import Route exposing (Route)

type alias Model =
    { session : Session
    , userName : String
    , password : String
    , againpassword : String
    }



-- init


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session, userName = "", password = "", againpassword = "" }
    , Cmd.none
    )


type Msg
    = SignInClicked
    | UserNameChanged String
    | PasswordChanged String
    | AgainPasswordChanged String
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SignInClicked ->
            ( model, Cmd.none )

        UserNameChanged userName ->
            ( { model | userName = userName }, Cmd.none )

        PasswordChanged pwd ->
            ( { model | password = pwd }, Cmd.none )

        AgainPasswordChanged apwd ->
            ( { model | againpassword = apwd }, Cmd.none )

        GotSession session ->
                    ( { model | session = session }
                    , Route.replaceUrl (Session.navKey session) Route.Home
                    )

view : Model -> { title : String, content : Html Msg }
view model =
     { title = "Login Window"
      , content =
            H.div []
                [ H.text "User Name Password input goes here...."
                ]
     }

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)
