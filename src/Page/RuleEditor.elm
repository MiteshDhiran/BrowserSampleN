module Page.RuleEditor exposing (DataTypeMetaInfo(..), Expression(..), Model, Msg, Operator(..), PropertyMetaInfo, ResourceMetaInfo, ResourceName(..), init, update, view)

import Html exposing (Html)
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
      , ruleExpression = BinOp And (BinOp Equal (String "A") (String "B")) (BinOp Equal (String "E") (String "F"))
      }
    , Cmd.none
    )


viewo : Model -> Html Msg
viewo model =
    Html.div [] (getHTML [] model.ruleExpression)


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Rule Editor"
    , content = Html.div [] (getHTML [] model.ruleExpression)
    }


getHTML : List (Html Msg) -> Expression -> List (Html Msg)
getHTML acc expression =
    case expression of
        BinOp operator lhs rhs ->
            --:: Html.text ("Operator" ++ Debug.toString operator)
            case operator of
                And ->
                    getHTML acc lhs
                        ++ [ Html.div []
                                ([ Html.text ("----" ++ Debug.toString operator) ]
                                    ++ getHTML acc rhs
                                )
                           ]

                {- [ Html.div [] (getHTML acc lhs ++ [ Html.text ("Operator New Line" ++ Debug.toString operator) ] ++ getHTML acc rhs) ] -}
                _ ->
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
