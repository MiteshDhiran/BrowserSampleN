module Username exposing (Username, createUserName, decoder, encode, toHtml, toString, urlParser)

import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Url.Parser



-- TYPES


type Username
    = Username String



-- create username from string


createUserName : String -> Username
createUserName userName =
    Username userName



-- CREATE


decoder : Decoder Username
decoder =
    Decode.map Username Decode.string



-- TRANSFORM


encode : Username -> Value
encode (Username username) =
    Encode.string username


toString : Username -> String
toString (Username username) =
    username


urlParser : Url.Parser.Parser (Username -> a) a
urlParser =
    Url.Parser.custom "USERNAME" (\str -> Just (Username str))


toHtml : Username -> Html msg
toHtml (Username username) =
    Html.text username
