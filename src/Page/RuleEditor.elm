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
    | PartialExpression Operator Expression Expression


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


quotedstringParser : Parser String
quotedstringParser =
    succeed identity
        |. token "$"
        |= loop [] stringHelp


stringHelp : List String -> Parser (Step (List String) String)
stringHelp revChunks =
    oneOf
        [ token "$"
            |> map (\_ -> Done (String.join "" (List.reverse revChunks)))
        , chompWhile isUninteresting
            |> getChompedString
            |> map (\chunk -> Loop (chunk :: revChunks))
        ]


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '$'


propertyParser : Parser PropertyMetaInfo
propertyParser =
    Parser.map
        (\( propertyName, parentProperties ) ->
            { propertyName = ( propertyName, [] ), propertyDataType = StringDataType }
        )
    <|
        succeed Tuple.pair
            |. spaces
            |. symbol "propertyName"
            |. spaces
            |. symbol "("
            |. spaces
            |= quotedstringParser
            |. spaces
            |. symbol "["
            |. spaces
            |= quotedstringParser
            |. spaces
            |. symbol "]"
            |. symbol ")"


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
        , Parser.map (\m -> Property m) <| propertyParser
        , succeed identity
            |. spaces
            |. operatorParser
            |. spaces
            |= lazy (\_ -> expressionParser)
            |. spaces
        ]


{-| . operatorParser
-}
expressionParser : Parser Expression
expressionParser =
    term
        |> andThen (expressionHelp [])



{- expressionHelp : List ( Expression, Operator ) -> Expression -> Parser Expression
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
-}


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


getExpFromPartailExpAndOpExp : Operator -> Expression -> Expression -> ( Operator, Expression ) -> Expression
getExpFromPartailExpAndOpExp peop pexp fexp ( op, exp ) =
    BinOp peop (BinOp op exp pexp) fexp


getBinaryExpression : (List ( Expression, Operator ) -> Expression -> Expression) -> ( Expression, Operator ) -> List ( Expression, Operator ) -> Expression -> Expression
getBinaryExpression finalizeFunc ( expr, operator ) otherRevOps finalExpr =
    case finalExpr of
        PartialExpression sop sexp sfexp ->
            finalizeFunc otherRevOps
                (getExpFromPartailExpAndOpExp sop sexp sfexp ( operator, expr ))

        _ ->
            finalizeFunc
                otherRevOps
                (BinOp operator expr finalExpr)


finalize : List ( Expression, Operator ) -> Expression -> Expression
finalize revOps finalExpr =
    case revOps of
        [] ->
            finalExpr

        ( expr, Equal ) :: otherRevOps ->
            getBinaryExpression finalize ( expr, Equal ) otherRevOps finalExpr

        ( expr, LessThan ) :: otherRevOps ->
            getBinaryExpression finalize ( expr, LessThan ) otherRevOps finalExpr

        ( expr, GreaterThan ) :: otherRevOps ->
            getBinaryExpression finalize ( expr, GreaterThan ) otherRevOps finalExpr

        ( expr, NotEqualTo ) :: otherRevOps ->
            getBinaryExpression finalize ( expr, NotEqualTo ) otherRevOps finalExpr

        ( expr, And ) :: otherRevOps ->
            case otherRevOps of
                [] ->
                    PartialExpression And expr finalExpr

                _ ->
                    BinOp And (finalize otherRevOps expr) finalExpr

        ( expr, Or ) :: otherRevOps ->
            case otherRevOps of
                [] ->
                    finalize otherRevOps (PartialExpression Or expr finalExpr)

                _ ->
                    BinOp Or (finalize otherRevOps expr) finalExpr



{-

   Debug.log
                   ("EQ:"
                       ++ Debug.toString expr
                       ++ "::O::"
                       ++ Debug.toString otherRevOps
                       ++ "::F::"
                       ++ Debug.toString finalExpr
                   )

      Debug.log
                      ("OR:"
                          ++ Debug.toString expr
                          ++ "::O::"
                          ++ Debug.toString otherRevOps
                          ++ "::F::"
                          ++ Debug.toString finalExpr
                      )

      BinOp
         Or
         (finalize otherRevOps expr)
         finalExpr
-}


binopParser : Parser Expression
binopParser =
    succeed BinOp
        |= operatorParser
        |. spaces
        |= digits
        |. spaces
        |= digits



{- BinOp
   Or
   (finalize otherRevOps expr)
   finalExpr

   case otherRevOps of
                   firstOtherOP :: restOtherOps ->
                       BinOp Or finalExpr <| finalize (restOtherOps ++ [ firstOtherOP ]) expr

                   _ ->
                       BinOp Or (finalize otherRevOps expr) finalExpr
-}


parse : String -> Result (List DeadEnd) Expression
parse str =
    run binopParser str



{-
   1 = 1 & 2 = 2
-}


parseExp : String -> Result (List DeadEnd) Expression
parseExp str =
    run expressionParser str



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
    Debug.toString (parseExp "1 = 14 && 2 == 24 && 3 == 34 || 4 == 44")



{- Debug.toString (parseExp "1 = 14 || 2 == 24 && 3 = 34 && 4 = 44 && 5 = 55") -}
{- Debug.toString (parseExp "1 = 1 && 2 = 2 && 3 = 3") -}
{- Debug.toString (parseExp "propertyName($Mitesh$[$Parent$]) && 1 = 1 && 2 = 2 && 3 = 3") -}


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

        PartialExpression operator lhs rhs ->
            Html.text "Partial Expression" :: acc

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
