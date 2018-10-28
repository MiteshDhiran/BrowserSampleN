module Page.RuleEditor exposing (DataTypeMetaInfo(..), Expression(..), Model, Msg, Operator(..), PropertyMetaInfo, ResourceMetaInfo, ResourceName(..), init, update, view)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Parser exposing (..)
import Session exposing (Session)
import Tree


type Operator
    = Equal
    | LessThan
    | GreaterThan
    | NotEqualTo
    | And
    | Or


type LeafExpression
    = NCharacter Char
    | NString String
    | NInteger Int
    | NFloat Float
    | NProperty PropertyMetaInfo


type Expression
    = Character Char
    | String String
    | Integer Int
    | Float Float
    | Property PropertyMetaInfo
    | BinOp Operator Expression Expression
    | SubExpression Expression
    | PartialExpression Operator Expression Expression


type NodeType
    = OperatorNode Operator
    | ExpressionNode Expression
    | RootNode


type alias Node =
    { nodeVal : NodeType, locationVal : Int, isEditable : Bool }


makeSingleton : Tree.Tree Node
makeSingleton =
    Tree.singleton { nodeVal = ExpressionNode (Integer 1), locationVal = 0, isEditable = False }


makeTreeWithSingleNode : Tree.Tree Node -> Tree.Tree Node
makeTreeWithSingleNode t =
    Tree.appendChild makeSingleton t


makeTreeWithIndex : Expression -> Maybe Node -> Tree.Tree Node
makeTreeWithIndex expression maybeEditableNode =
    let
        editableIndex =
            case maybeEditableNode of
                Just editableNode ->
                    editableNode.locationVal

                Nothing ->
                    -1
    in
    makeTree expression (Tree.singleton { nodeVal = RootNode, locationVal = -1, isEditable = False })
        |> Tree.indexedMap (\idx val -> { val | locationVal = idx, isEditable = editableIndex == idx })


makeTree : Expression -> Tree.Tree Node -> Tree.Tree Node
makeTree exp t =
    case exp of
        Character c ->
            Tree.appendChild (Tree.singleton { nodeVal = ExpressionNode exp, locationVal = 0, isEditable = False }) t

        Float f ->
            Tree.appendChild (Tree.singleton { nodeVal = ExpressionNode exp, locationVal = 0, isEditable = False }) t

        Integer i ->
            Tree.appendChild (Tree.singleton { nodeVal = ExpressionNode exp, locationVal = 0, isEditable = False }) t

        Property pi ->
            Tree.appendChild (Tree.singleton { nodeVal = ExpressionNode exp, locationVal = 0, isEditable = False }) t

        BinOp op lhs rhs ->
            let
                operatorTreeNodeOf =
                    Tree.tree { nodeVal = OperatorNode op, locationVal = 0, isEditable = False } (Tree.children (makeTree lhs (Tree.singleton { nodeVal = RootNode, locationVal = -1, isEditable = False })) ++ Tree.children (makeTree rhs (Tree.singleton { nodeVal = RootNode, locationVal = -1, isEditable = False })))
            in
            Tree.appendChild operatorTreeNodeOf t

        _ ->
            t



{-
   type Expression
       = Character Char
       | String String
       | Integer Int
       | Float Float
       | Property PropertyMetaInfo
       | BinOp Operator Expression Expression
       | SubExpression Expression
       | PartialExpression Operator Expression Expression
-}
{- type alias Node = Expression -}


type Exp
    = CharacterE Char
    | StringE String
    | IntegerE Int
    | FloatE Float



{-
   type BinaryTree exp
       = Empty
       | Node Operator (BinaryTree Exp) (BinaryTree Exp)
-}


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
    , editableExpressionTree : Tree.Tree Node
    , editableNode : Maybe Node

    {- , binaryTree : BinaryTree Expression -}
    }


type Msg
    = AddRule Operator Expression Expression
    | NodeClick Node
    | UpdateNodeValue Node String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddRule operator lhs rhs ->
            ( { model | ruleExpression = addBinOp operator lhs rhs And Equal (Integer 7) (Integer 7) }, Cmd.none )

        NodeClick node ->
            ( { model | editableExpressionTree = markNodeEditable model.editableExpressionTree (Just node) }, Cmd.none )

        UpdateNodeValue node value ->
            let
                editableNodeIndex =
                    node.locationVal

                editedExpreesionValue =
                    case node.nodeVal of
                        OperatorNode op ->
                            OperatorNode op

                        ExpressionNode exp ->
                            case exp of
                                Integer _ ->
                                    ExpressionNode (Integer (Maybe.withDefault 0 (String.toInt value)))

                                String _ ->
                                    ExpressionNode (String value)

                                Float _ ->
                                    ExpressionNode (Float (Maybe.withDefault 0 (String.toFloat value)))

                                _ ->
                                    ExpressionNode exp

                        {- ExpressionNode (Integer (Maybe.withDefault 0 (String.toInt value))) -}
                        RootNode ->
                            node.nodeVal

                newNode =
                    { nodeVal = editedExpreesionValue, locationVal = editableNodeIndex, isEditable = True }

                newTree =
                    Tree.indexedMap
                        (\idx val ->
                            if idx == editableNodeIndex then
                                newNode

                            else
                                val
                        )
                        model.editableExpressionTree
            in
            ( { model | editableExpressionTree = newTree }, Cmd.none )


addBinOp : Operator -> Expression -> Expression -> Operator -> Operator -> Expression -> Expression -> Expression
addBinOp currentOperator currentLHSExpression currentRHSExpression newJoinOperator newOperator newLHSExpression newRHSExpression =
    BinOp newJoinOperator (BinOp currentOperator currentLHSExpression currentRHSExpression) (BinOp newOperator newLHSExpression newRHSExpression)



{- init : Session -> Model
   = 1 1 & = 2 2
   1 = 1  & 2 = 2
-}


convertNodeToString : NodeType -> String
convertNodeToString nodeType =
    case nodeType of
        ExpressionNode expression ->
            convertExpressionToString expression

        OperatorNode operator ->
            Debug.toString operator

        RootNode ->
            "root"


convertExpressionToString : Expression -> String
convertExpressionToString expression =
    case expression of
        Integer int ->
            String.fromInt int

        Float ft ->
            String.fromFloat ft

        Character chr ->
            String.fromChar chr

        Property propMetainfo ->
            Debug.toString propMetainfo

        _ ->
            "REST"


labelToHtml : Node -> Html Msg
labelToHtml { nodeVal, locationVal, isEditable } =
    let
        node =
            { nodeVal = nodeVal
            , locationVal = locationVal
            , isEditable = isEditable
            }
    in
    case isEditable of
        False ->
            Html.div
                [ Html.Events.onClick (NodeClick node)
                ]
                [ Html.text ("[" ++ String.fromInt locationVal ++ "]" ++ convertNodeToString nodeVal)
                ]

        True ->
            Html.div
                [ Html.Events.onClick (NodeClick node)
                ]
                [ Html.input
                    [ Html.Attributes.value (convertNodeToString nodeVal)
                    , Html.Events.onInput <| UpdateNodeValue node
                    ]
                    []
                ]


toListItems : Html Msg -> List (Html Msg) -> Html Msg
toListItems label children =
    case children of
        [] ->
            Html.li [] [ label ]

        _ ->
            Html.li []
                [ label
                , Html.ul [] children
                ]



{-
   getDefaultView : Html Msg
   getDefaultView =
       Html.text (Debug.toString getDefaultTree)
-}


getDefaultTreeView : Expression -> Maybe Node -> Html Msg
getDefaultTreeView expression maybeNode =
    getDefaultTree expression maybeNode
        |> Tree.restructure labelToHtml toListItems
        |> (\root -> Html.ul [] [ root ])


getViewOfTree : Tree.Tree Node -> Html Msg
getViewOfTree expressionTree =
    expressionTree
        |> Tree.restructure labelToHtml toListItems
        |> (\root -> Html.ul [] [ root ])


getDefaultExpression : Expression
getDefaultExpression =
    case parseExp "1 = 14 && 2 == 24 && 3 == 34 || 4 == 44" of
        Ok value ->
            value

        _ ->
            BinOp And
                (SubExpression (BinOp And (BinOp Equal (String "A") (String "B")) (BinOp Equal (String "E") (String "F"))))
                (SubExpression
                    (BinOp And (BinOp Equal (String "1") (String "11")) (BinOp Equal (String "2") (String "22")))
                )


getDefaultTree : Expression -> Maybe Node -> Tree.Tree Node
getDefaultTree expression maybeNode =
    makeTreeWithIndex expression maybeNode


markNodeEditable : Tree.Tree Node -> Maybe Node -> Tree.Tree Node
markNodeEditable editableExpressionTree maybeEditableNode =
    case maybeEditableNode of
        Just node ->
            Tree.indexedMap (\idx val -> { val | isEditable = node.locationVal == idx }) editableExpressionTree

        Nothing ->
            editableExpressionTree


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , ruleExpression =
            case parseExp "1 = 14 && 2 == 24 && 3 == 34 || 4 == 44" of
                Ok value ->
                    value

                _ ->
                    BinOp And
                        (SubExpression (BinOp And (BinOp Equal (String "A") (String "B")) (BinOp Equal (String "E") (String "F"))))
                        (SubExpression
                            (BinOp And (BinOp Equal (String "1") (String "11")) (BinOp Equal (String "2") (String "22")))
                        )
      , editableExpressionTree = getDefaultTree getDefaultExpression Nothing
      , editableNode = Nothing
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
{- https://package.elm-lang.org/packages/peterszerzo/elm-arborist/latest/ -}
{- https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/ -}


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Rule Editor"
    , content = Html.div [] (getHTML [] 1 model.ruleExpression ++ [ Html.div [] [ Html.text getParsedExpString ] ] ++ [ Html.div [] [ getViewOfTree model.editableExpressionTree ] ])
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
            case operator of
                And ->
                    (getHTML acc indent lhs
                        ++ [ Html.div []
                                ([ Html.text (getSpaces (indent + 1) ++ Debug.toString operator) ]
                                    ++ getHTML acc (indent + 1) rhs
                                )
                           ]
                    )
                        ++ [ Html.button [ Html.Events.onClick (AddRule operator lhs rhs) ] [ Html.text " Click me to append rule" ] ]

                Or ->
                    getHTML acc indent lhs
                        ++ [ Html.div []
                                ([ Html.text (getSpaces (indent + 1) ++ Debug.toString operator) ]
                                    ++ getHTML acc (indent + 1) rhs
                                )
                           ]

                _ ->
                    getHTML acc indent lhs ++ [ Html.text (Debug.toString operator) ] ++ getHTML acc indent rhs

        PartialExpression operator lhs rhs ->
            Html.text "Partial Expression" :: acc

        Property propMetainfo ->
            Html.text ("PropertyName" ++ Tuple.first propMetainfo.propertyName) :: acc

        String str ->
            Html.text ("String Constant" ++ str) :: acc

        Integer intVal ->
            Html.text (Debug.toString intVal) :: acc

        Float ft ->
            Html.text ("Float Constant" ++ "ft") :: acc

        Character chr ->
            Html.text ("Character Constant" ++ "chr") :: acc
