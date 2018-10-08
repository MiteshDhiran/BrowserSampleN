module Session exposing (Session(..), changes, createNewSession, fromViewer, getUserName, navKey)

import Api exposing (..)
import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import Username
import Viewer exposing (..)


type Session
    = LoggedIn Nav.Key Viewer
    | Guest Nav.Key


createNewSession : Nav.Key -> String -> String -> Session
createNewSession key auserName atoken =
    LoggedIn key (Viewer.createViewerFromUserNameToken auserName atoken)


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key


getUserName : Session -> String
getUserName session =
    case session of
        LoggedIn k viewer ->
            Username.toString <| Viewer.username <| viewer

        Guest _ ->
            "Guest User"


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    Api.viewerChanges (\maybeViewer -> toMsg (fromViewer key maybeViewer)) Viewer.decoder


fromViewer : Nav.Key -> Maybe Viewer -> Session
fromViewer key maybeViewer =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    case maybeViewer of
        Just viewerVal ->
            LoggedIn key viewerVal

        Nothing ->
            Guest key
