module Page exposing (..)

import Browser exposing (Document)
import Html exposing (Html, a, button, div, footer, i, img, li, nav, p, span, text, ul)
import Html.Attributes exposing (class, classList, href, style)
import Html.Events exposing (onClick)
import Route exposing (Route)
import Session exposing (Session)

type Page
    = Other
    | Home
    | Login


view : Session -> Page -> { title : String, content : Html msg } -> Document msg
view session page { title, content } =
    { title = title ++ " - XYZ Company"
    , body = viewHeader page  session :: content :: []
    }

viewHeader : Page -> Session -> Html msg
viewHeader page session =
    Html.div[][ Html.text "Header goes here"]

