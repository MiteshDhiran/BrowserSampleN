import Url
import Url.Parser as P exposing (Parser,fragment, (</>), (<?>), int, map, oneOf, s, string)
import Url.Parser.Query as Query
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Docs =
  (String, Maybe String)

type Route
  = Topic String
  | Blog Int
  | User String
  | Comment String Int
  | BlogPost Int String
  | BlogQuery (Maybe String)

docsParser : Parser (Docs -> a) a
docsParser =
  P.map Tuple.pair (string </> fragment identity)

routeParser : Parser (Route -> a) a
routeParser =
  oneOf
    [ P.map Topic   (P.s "topic" </> string)
    , P.map Blog    (P.s "blog" </> int)
    , P.map User    (P.s "user" </> string)
    , P.map Comment (P.s "user" </> string </> P.s "comment" </> int)
    , P.map BlogPost  (P.s "blog" </> int </> string) 
    , P.map BlogQuery (P.s "blog" <?> Query.string "q")
    ]
    

-- /topic/pottery        ==>  Just (Topic "pottery")
-- /topic/collage        ==>  Just (Topic "collage")
-- /topic/               ==>  Nothing

-- /blog/42              ==>  Just (Blog 42)
-- /blog/123             ==>  Just (Blog 123)
-- /blog/mosaic          ==>  Nothing

-- /user/tom/            ==>  Just (User "tom")
-- /user/sue/            ==>  Just (User "sue")
-- /user/bob/comment/42  ==>  Just (Comment "bob" 42)
-- /user/sam/comment/35  ==>  Just (Comment "sam" 35)
-- /user/sam/comment/    ==>  Nothing
-- /user/                ==>  Nothing

-- /blog/12/the-history-of-chairs
-- /blog/13/the-endless-september
-- /blog/14/whale-facts
-- /blog/
-- /blog?q=whales
-- /blog?q=seiza

-- /blog/14/whale-facts  ==>  Just (BlogPost 14 "whale-facts")
-- /blog/14              ==>  Nothing
-- /blog/whale-facts     ==>  Nothing
-- /blog/                ==>  Just (BlogQuery Nothing)
-- /blog                 ==>  Just (BlogQuery Nothing)
-- /blog?q=chabudai      ==>  Just (BlogQuery (Just "chabudai"))
-- /blog/?q=whales       ==>  Just (BlogQuery (Just "whales"))
-- /blog/?query=whales   ==>  Just (BlogQuery Nothing)

type Model 
  = MainPage
  | BlogPage  
  | UserPage


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClickedA
    }

