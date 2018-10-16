module Page.RuleEditor exposing (DataTypeMetaInfo(..), Expression(..), Model, Msg, Operator(..), PropertyMetaInfo, ResourceMetaInfo, ResourceName(..), init, update, view)

import Html exposing (Html)
import Parser exposing (..)
import Session exposing (Session)


type Operator
    = Equal
    | LessThan
    | GreaterThan
    | NotEqualTo
    | And
    | Or


type Expression
    = Character Char
    | String String
    | Integer Int
    | Float Float
    | Property PropertyMetaInfo
    | BinOp Operator Expression Expression
    | SubExpression Expression


type alias PropertyMetaInfo =
    { propertyName : ( String, List String )
    , propertyDataType : DataTypeMetaInfo
    }


type DataTypeMetaInfo
    = StringDataType
    | IntDataType
    | FloatDataType


type ResourceName
    = ResourceName String


type alias ResourceMetaInfo =
    { resourceName : ResourceName
    , properties : List PropertyMetaInfo
    }


type alias Model =
    { session : Session
    , ruleExpression : Expression
    }


type Msg
    = AddRule


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddRule ->
            ( model, Cmd.none )



{- init : Session -> Model -}


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , ruleExpression =
            BinOp And
                (SubExpression (BinOp And (BinOp Equal (String "A") (String "B")) (BinOp Equal (String "E") (String "F"))))
                (SubExpression
                    (BinOp And (BinOp Equal (String "1") (String "11")) (BinOp Equal (String "2") (String "22")))
                )
      }
    , Cmd.none
    )


digits : Parser Expression
digits =
    number
        { int = Just Integer
        , hex = Just Integer
        , octal = Nothing
        , binary = Nothing
        , float = Just Float
        }


operatorParser : Parser Operator
operatorParser =
    oneOf
        [ map (\_ -> Equal) (symbol "=")
        , map (\_ -> GreaterThan) (symbol ">")
        , map (\_ -> LessThan) (symbol "<")
        , map (\_ -> NotEqualTo) (symbol "!=")
        , map (\_ -> And) (symbol "&")
        , map (\_ -> Or) (symbol "|")
        ]



{-
   " = 1 2 "
   BinOp Equal 1 2
-}


{-| Every expression starts with a term. After that, it may be done, or there
may be a `+` or `*` sign and more math.
-}



{-
   expression : Parser Expression
   expression =
       term
           |> andThen (expressionHelp [])
-}
{-
   expression : Parser Expression
   expression =
       operator
           |> andThen (expressionHelp [])
-}


binopParser : Parser Expression
binopParser =
    succeed BinOp
        |= operatorParser
        |. spaces
        |= digits
        |. spaces
        |= digits


parse : String -> Result (List DeadEnd) Expression
parse string =
    run binopParser string



{-
   https://github.com/elm/parser

      expression : Parser s Expression
      expression
-}


viewo : Model -> Html Msg
viewo model =
    Html.div [] (getHTML [] 0 model.ruleExpression)


getParsedExpressionString : String
getParsedExpressionString =
    Debug.toString (parse "= 1 2")


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Rule Editor"
    , content = Html.div [] (getHTML [] 0 model.ruleExpression ++ [ Html.div [] [ Html.text getParsedExpressionString ] ])
    }


getSpaces : Int -> String
getSpaces numberOfSpaces =
    String.repeat numberOfSpaces "-"


getHTML : List (Html Msg) -> Int -> Expression -> List (Html Msg)
getHTML acc indent expression =
    case expression of
        SubExpression subExp ->
            [ Html.div []
                ([ Html.text "(" ]
                    ++ getHTML acc indent subExp
                    ++ [ Html.text ")" ]
                )
            ]

        BinOp operator lhs rhs ->
            --:: Html.text ("Operator" ++ Debug.toString operator)
            case operator of
                And ->
                    let
                        newIndent =
                            indent + 1
                    in
                    getHTML acc indent lhs
                        ++ [ Html.div []
                                ([ Html.text (getSpaces newIndent ++ Debug.toString operator) ]
                                    ++ getHTML acc newIndent rhs
                                )
                           ]

                {- [ Html.div [] (getHTML acc lhs ++ [ Html.text ("Operator New Line" ++ Debug.toString operator) ] ++ getHTML acc rhs) ] -}
                _ ->
                    getHTML acc indent lhs ++ [ Html.text ("Operator" ++ Debug.toString operator) ] ++ getHTML acc indent rhs

        Property propMetainfo ->
            Html.text ("PropertyName" ++ Tuple.first propMetainfo.propertyName) :: acc

        String str ->
            Html.text ("String Constant" ++ str) :: acc

        Integer intVal ->
            Html.text ("Integer Constant" ++ "intVal") :: acc

        Float ft ->
            Html.text ("Float Constant" ++ "ft") :: acc

        Character chr ->
            Html.text ("Character Constant" ++ "chr") :: acc
