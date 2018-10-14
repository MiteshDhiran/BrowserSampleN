module Page.RuleEditor exposing (DataTypeMetaInfo(..), Expression(..), Operator(..), PropertyMetaInfo, ResourceMetaInfo, ResourceName(..))

import Html exposing (Html)


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
    { ruleExpression : Expression
    }


type Msg
    = AddRule


view : Model -> Html Msg
view model =
    case model.ruleExpression of
        BinOp op lex rex ->
            Html.div [] []

        _ ->
            Html.div [] []


getHTML : List (Html Msg) -> Expression -> List (Html Msg)
getHTML acc expression =
    case expression of
        BinOp operator lhs rhs ->
            --:: Html.text ("Operator" ++ Debug.toString operator)
            getHTML acc lhs ++ [ Html.text ("Operator" ++ Debug.toString operator) ] ++ getHTML acc rhs

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
