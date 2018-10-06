module Page.Login exposing (Model, Msg(..), init, subscriptions, update, view)

import Api exposing (..)
import Browser.Navigation as Nav
import Html as H exposing (..)
import Html.Attributes
import Html.Events
import Route exposing (Route)
import Session exposing (..)
import Tuple



--import Viewer exposing (Viewer, cred, decoder, minPasswordChars, store, username)


type alias Model =
    { session : Session
    , userName : String
    , password : String
    , againpassword : String
    }



-- init


init : Session -> ( Model, Cmd msg )
init session =
    let
        userName =
            Session.getUserName session
    in
    ( { session = session, userName = userName, password = "", againpassword = "" }
    , Cmd.none
    )


type Msg
    = SignInClicked
    | UserNameChanged String
    | PasswordChanged String
    | AgainPasswordChanged String
    | GotSession Session
    | NavigateToHomePage
    | Redirect


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SignInClicked ->
            ( { model | session = Session.createNewSession (Session.navKey model.session) model.userName "" }, Cmd.none )

        UserNameChanged userName ->
            ( { model | userName = userName }, Cmd.none )

        PasswordChanged pwd ->
            ( { model | password = pwd }, Cmd.none )

        AgainPasswordChanged apwd ->
            ( { model | againpassword = apwd }, Cmd.none )

        GotSession session ->
            ( { model | session = session }
              --, Route.replaceUrl (Session.navKey session) Route.Home
            , Cmd.none
            )

        --session = LoggedIn (Session.navKey model.session) createViewerFromUserNameToken model.userName ""
        NavigateToHomePage ->
            ( { model | session = Session.createNewSession (Session.navKey model.session) model.userName "" }
            , Cmd.batch
                [ Nav.back
                    (navKey model.session)
                    1
                ]
            )

        Redirect ->
            Debug.log "Redirect command will never  be handled here as its being handled in parent"
                ( model, Cmd.none )


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Login Window"
    , content =
        H.div []
            [ H.text "User Name Password input goes here...."
            , H.input
                [ Html.Attributes.value model.userName
                , Html.Events.onInput UserNameChanged
                ]
                []
            , H.button [ Html.Events.onClick NavigateToHomePage ]
                [ H.text "Back to Home Page" ]
            , H.button [ Html.Events.onClick SignInClicked ]
                [ H.text "Sign In" ]
            , H.button [ Html.Events.onClick Redirect ]
                [ H.text "Redirect" ]
            ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Debug.log
        ("Login module subscriptions"
            ++ Debug.toString model
        )
        Session.changes
        GotSession
        (Session.navKey model.session)
