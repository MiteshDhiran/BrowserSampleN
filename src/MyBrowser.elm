import Browser
import MyColor 
import Browser.Navigation as Nav
import Css exposing (Color, hex, margin, rgb, underline, underline)
import Html as H exposing (..)
import Html.Attributes exposing (..)
import Url
import Url.Parser as P exposing (Parser,parse,fragment, (</>), (<?>), int, map, oneOf, s, string)
import Url.Parser.Query as Query
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Style exposing(..)
import Style.Color as StyleColor exposing (..)
import Style.Font as Font
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Input
import Element.Events

--https://mdgriffith.gitbooks.io/style-elements/content/

sansSerif =
    [ Font.font "helvetica"
    , Font.font "arial"
    , Font.font "sans-serif"
    ]


type ButtonType
    = One
    | Two
    | Three

type ButtonStyles
        = Active
        | Disabled

type MyStyles
    = None
    | Title
    | LinkStyle
    | NavOption
    | Button ButtonStyles

stylesheet =
    Style.styleSheet
        [ Style.style Title
            [
                StyleColor.text MyColor.darkGrey
                , Font.size 20 -- all units given as px
                , Font.typeface
                    [ Font.font "Helvetica"
                    , Font.font "Comic Sans"
                    , Font.font "Papyrus"
                    ]
            ]
          , Style.style LinkStyle
            [
                StyleColor.text MyColor.blue
                                , Font.size 25 -- all units given as px
                                , Font.typeface
                                    [ Font.font "Helvetica"
                                    , Font.font "Comic Sans"
                                    , Font.font "Papyrus"
                                    ]
            ]
          , Style.style None []
          , Style.style NavOption
                [ Font.size 16
                 ,Font.typeface sansSerif
                ]
          , Style.style (Button Active)
                          [ StyleColor.background MyColor.blue
                          ]
                      , Style.style (Button Disabled)
                          [ StyleColor.background MyColor.grey
                          ]
        ]

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view =  sview
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClickedA
    }

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , content : Maybe String
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key url Nothing, Cmd.none )

type Msg
  = LinkClickedA Browser.UrlRequest
  | UrlChanged Url.Url
  | ButtonClicked ButtonType


type Route
  = Home
  | Reviews String
  | NotFound


routeParser : Parser (Route -> a) a
routeParser =
  oneOf
    [ P.map Home   (P.s "home")
    , P.map Reviews    (P.s "reviews" </> string)
    ]

toRoute : String -> Route
toRoute string =
  case Url.fromString string of
    Nothing ->
      NotFound

    Just url ->
      Maybe.withDefault NotFound (P.parse routeParser url)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClickedA urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          let
            pth = url.path
            parsedContent = toRoute <| Url.toString <| url
          in
          ( {model | content = Just <| Debug.toString <| parsedContent }, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )

    ButtonClicked btnType ->
        ( { model | content = Just "Button Clicked" }, Cmd.none)
 
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

type alias DocumentS msg=
 { title : String
  ,body : List (HS.Html msg)
 }

toDocumentMsg : DocumentS (Msg) -> (Browser.Document (Msg))
toDocumentMsg styledDoc =
    { title = styledDoc.title, body = List.map (\m -> toUnstyled m) styledDoc.body }


stoDocumentMsg : H.Html (Msg) -> (Browser.Document (Msg))
stoDocumentMsg helms =
    { title = "ABC", body = [helms] }


theme : { secondary : Css.Color, primary : Css.Color }
theme =
    { primary = hex "55af6a"
    , secondary = Css.rgb 250 240 230
    }


btn : List (HS.Attribute msg) -> List (HS.Html msg) -> HS.Html msg
btn =
    styled HS.button
        [ margin (Css.px 12)
        , Css.color (Css.rgb 250 250 250)
        , Css.hover
            [ Css.backgroundColor theme.primary
            , Css.textDecoration Css.underline
            ]
        ]

{-
type alias Document msg =
  { title : String
  , body : List (Html msg)
  }
-}

sview : Model -> Browser.Document Msg
sview model =
    (view model) |> stoDocumentMsg

--paragraph style attrs children
view : Model -> H.Html (Msg)
view model =
            Element.layout stylesheet <|
            -- An el is the most basic element, like a <div>
            --Element.el Title [] (Element.text "hello world!")
            Element.column Title []
                [ el Title [] (Element.text "Hello")
                , el Title [] (Element.text "World")
                , el Title [] (Element.text "!")
                , viewLinkEE "/home"
                , viewLinkEE "/profile"
                , viewLinkEE "/reviews/the-century-of-the-self"
                , viewLinkEE "/reviews/public-opinion"
                , viewLinkEE "/reviews/shah-of-shahs"
                , Element.paragraph Title [] [ Element.text <| Maybe.withDefault ""  model.content]
                , Element.navigation None
                         [spread, Element.Attributes.width (px 800), paddingXY 80 20 ]
                         { name = "Main Navigation"
                         , options =
                             [ link "/profile" (el None [] (Element.text "profile"))
                             , link "/logout" (el None [] (Element.text "logout"))
                             ]
                         }

                , Element.row None
                          [ spread, paddingXY 80 20 ]
                          [ el Title [] (Element.text "Style Elements")
                          , row None
                              [ spacing 20, alignBottom ]
                              [ el NavOption [] (link "/share" (el None [] (Element.text "share")))
                              , el NavOption [] (link "/about" (el None [] (Element.text "about")))
                              , el NavOption [] (link "/user profile" (el None [] (Element.text "user profile")))
                              ]
                          ]
                , Element.row None [] [Element.button (Button Active) [ Element.Events.onClick (ButtonClicked One) ] (Element.text "Press Me !!!")]

                ]

{-button : style -> List (Attribute variation msg) -> Element style variation msg -> Element style variation msg-}
{-
cview : Model -> Browser.Document Msg
cview model =
    (view model) |> toDocumentMsg

view : Model -> { body : List (HS.Html msg), title : String }
view model =
  { title = "URL Interceptor"
  , body =
      [ HS.text "The current URL is: "
      , HS.b [] [ HS.text (Url.toString model.url) ]
      , HS.ul []
          [ viewLinkS "/home"
          , viewLinkS "/profile"
          , viewLinkS "/reviews/the-century-of-the-self"
          , viewLinkS "/reviews/public-opinion"
          , viewLinkS "/reviews/shah-of-shahs"
          ]
      , btn [ ] [ HS.text "Click me!" ]
       , HS.text model.content
      ]
  }
  -}

{-
view : Model -> Browser.Document Msg
view model =
  { title = "URL Interceptor"
  , body =
      [ H.text "The current URL is: "
      , H.b [] [ H.text (Url.toString model.url) ]
      , H.ul []
          [ viewLink "/home" 
          , viewLink "/profile" 
          , viewLink "/reviews/the-century-of-the-self"
          , viewLink "/reviews/public-opinion" 
          , viewLink "/reviews/shah-of-shahs"
          ]
       , H.text model.content
      ]
  }
-}


viewLink : String -> H.Html msg
viewLink path =
  H.li [] [ H.a [ Html.Attributes.href path ] [ H.text path ] ]

viewLinkS : String -> HS.Html msg
viewLinkS path =
  HS.li [] [ HS.a [ href path ] [ HS.text path ] ]

-- el Title [] (Element.text "!")
viewLinkE : String -> Element MyStyles variation msg
viewLinkE path =
  Element.html (H.text path)

{-link : String -> Element style variation msg -> Element style variation msg-}

viewLinkEE : String -> Element MyStyles variation msg
viewLinkEE path =
    link path <| el LinkStyle [] (Element.text path)
  --Element.html ( H.a [ Html.Attributes.href path ] [ H.text path ] )

