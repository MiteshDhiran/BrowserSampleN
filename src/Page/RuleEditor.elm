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


type Exp
    = CharacterE Char
    | StringE String
    | IntegerE Int
    | FloatE Float


type BinaryTree exp
    = Empty
    | Node Operator (BinaryTree Exp) (BinaryTree Exp)


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

    {- , binaryTree : BinaryTree Expression -}
    }


type Msg
    = AddRule


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddRule ->
            ( model, Cmd.none )



{- init : Session -> Model
   = 1 1 & = 2 2
   1 = 1  & 2 = 2
-}


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , ruleExpression =
            BinOp And
                (SubExpression (BinOp And (BinOp Equal (String "A") (String "B")) (BinOp Equal (String "E") (String "F"))))
                (SubExpression
                    (BinOp And (BinOp Equal (String "1") (String "11")) (BinOp Equal (String "2") (String "22")))
                )

      {- , binaryTree = Node (Equal (IntegerE 1) (IntegerE 1)) -}
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
        , map (\_ -> And) (symbol "&&")
        , map (\_ -> Or) (symbol "||")
        ]



{-
   " = 1 2 "
   BinOp Equal 1 2
-}


term : Parser Expression
term =
    oneOf
        [ digits
        , succeed identity
            |. spaces
            |. operatorParser
            |. spaces
            |= lazy (\_ -> expressionParser)
            |. spaces
        ]


expressionParser : Parser Expression
expressionParser =
    term
        |> andThen (expressionHelp [])


expressionHelp : List ( Expression, Operator ) -> Expression -> Parser Expression
expressionHelp revOps lhs =
    oneOf
        [ succeed Tuple.pair
            |. spaces
            |= operatorParser
            |. spaces
            |= term
            |. spaces
            |> andThen (\( opr, newExpr ) -> expressionHelp (( lhs, opr ) :: revOps) newExpr)
        , lazy (\_ -> succeed (finalize revOps lhs))
        ]


finalize : List ( Expression, Operator ) -> Expression -> Expression
finalize revOps finalExpr =
    case revOps of
        [] ->
            finalExpr

        ( expr, Equal ) :: otherRevOps ->
            finalize otherRevOps (BinOp Equal expr finalExpr)

        ( expr, LessThan ) :: otherRevOps ->
            finalize otherRevOps (BinOp LessThan expr finalExpr)

        ( expr, GreaterThan ) :: otherRevOps ->
            finalize otherRevOps (BinOp GreaterThan expr finalExpr)

        ( expr, NotEqualTo ) :: otherRevOps ->
            finalize otherRevOps (BinOp NotEqualTo expr finalExpr)

        ( expr, And ) :: otherRevOps ->
            BinOp And (finalize otherRevOps expr) finalExpr

        ( expr, Or ) :: otherRevOps ->
            BinOp Or (finalize otherRevOps expr) finalExpr


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
   1 = 1 & 2 = 2
-}


parseExp : String -> Result (List DeadEnd) Expression
parseExp string =
    run expressionParser string



{-

   " = 1 2 && = 1 3"
      https://github.com/elm/parser

         expression : Parser s Expression
         expression
-}


viewo : Model -> Html Msg
viewo model =
    Html.div [] (getHTML [] 0 model.ruleExpression)


getParsedExpressionString : String
getParsedExpressionString =
    Debug.toString (parse "= 11 21 & 2 3")


getParsedExpString : String
getParsedExpString =
    Debug.toString (parseExp "1 = 1 && 2 = 2 && 3 = 3")


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Rule Editor"
    , content = Html.div [] (getHTML [] 0 model.ruleExpression ++ [ Html.div [] [ Html.text getParsedExpString ] ])
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
