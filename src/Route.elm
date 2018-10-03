module Route exposing (Route(..), fromUrl, href, parser, replaceUrl, routeToString)

import Browser.Navigation as Nav
import Html
import Html.Attributes
import Html.Events
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


type Route
    = Home
    | Login
    | Root



{-
   parser : Parser (Route -> a) a
   parser =
    oneOf
        [
           Parser.map Home Parser.top
         , Parser.map Login (s "login")
        ]
-}


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Login (s "login")
        , Parser.map Home Parser.top
        ]


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    [ "home" ]

                Root ->
                    []

                Login ->
                    [ "login" ]
    in
    "#/" ++ String.join "/" pieces



-- PUBLIC HELPERS


href : Route -> Html.Attribute msg
href targetRoute =
    Html.Attributes.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    let
        r =
            { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
                |> Parser.parse parser
    in
    Debug.log ("URL" ++ Debug.toString url ++ "Route" ++ Debug.toString r)
        r
