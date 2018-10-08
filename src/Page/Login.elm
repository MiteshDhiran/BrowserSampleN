module Page.Login exposing (Model, Msg(..), init, subscriptions, update, view)

import Api exposing (..)
import Browser.Navigation as Nav
import Html as H exposing (..)
import Html.Attributes
import Html.Events
import Http
import Json.Encode as Encode
import Route exposing (Route)
import Session exposing (..)
import Tuple
import Viewer exposing (Viewer)



--import Viewer exposing (Viewer, cred, decoder, minPasswordChars, store, username)


type ValidatedField
    = Email
    | Password


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


type alias Model =
    { session : Session
    , userName : String
    , password : String
    , againpassword : String
    , problems : List Problem
    }



-- init


init : Session -> ( Model, Cmd msg )
init session =
    let
        userName =
            Session.getUserName session
    in
    ( { session = session, userName = userName, password = "", againpassword = "", problems = [] }
    , Cmd.none
    )


type Msg
    = SignInClicked
    | UserNameChanged String
    | PasswordChanged String
    | AgainPasswordChanged String
    | GotSession Session
    | NavigateToHomePage
    | CompletedLogin (Result Http.Error Viewer)



{- SignInClicked ->
   ( { model | session = Session.createNewSession (Session.navKey model.session) model.userName "" }
   , Cmd.batch
       [ Nav.back
           (navKey model.session)
           1
       ]
   )
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SignInClicked ->
            ( { model | session = Session.createNewSession (Session.navKey model.session) model.userName model.password }
            , Http.send CompletedLogin (login model)
            )

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

        NavigateToHomePage ->
            ( model
            , Cmd.batch
                [ Nav.back
                    (navKey model.session)
                    1
                ]
            )

        CompletedLogin (Err error) ->
            let
                serverErrors =
                    Api.decodeErrors error
                        |> List.map ServerError
            in
            Debug.log ("Error occurred in CompletedLogin" ++ Debug.toString error)
                ( { model | problems = List.append model.problems serverErrors }
                , Cmd.none
                )

        CompletedLogin (Ok viewer) ->
            Debug.log ("CompletedLogin Success" ++ Debug.toString viewer)
                ( { model | session = LoggedIn (Session.navKey model.session) viewer }
                , Nav.back (navKey model.session) 1
                )


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
            , H.text "Enter password"
            , H.input
                [ Html.Attributes.value model.password
                , Html.Events.onInput PasswordChanged
                ]
                []
            , H.button [ Html.Events.onClick NavigateToHomePage ]
                [ H.text "Back to Home Page" ]
            , H.button [ Html.Events.onClick SignInClicked ]
                [ H.text "Sign In" ]
            ]
    }


login : Model -> Http.Request Viewer
login model =
    let
        user =
            Encode.object
                [ ( "email", Encode.string model.userName )
                , ( "password", Encode.string model.password )
                ]

        body =
            Encode.object [ ( "user", user ) ]
                |> Http.jsonBody
    in
    Api.login body Viewer.decoder


createLoginJSON : Model -> Http.Body
createLoginJSON model =
    let
        user =
            Encode.object
                [ ( "email", Encode.string model.userName )
                , ( "password", Encode.string model.password )
                ]

        body =
            Encode.object [ ( "user", user ) ]
                |> Http.jsonBody
    in
    body


subscriptions : Model -> Sub Msg
subscriptions model =
    Debug.log
        ("Login module subscriptions"
            ++ Debug.toString model
        )
        Session.changes
        GotSession
        (Session.navKey model.session)
