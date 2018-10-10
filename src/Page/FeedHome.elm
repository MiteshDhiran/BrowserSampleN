module Page.FeedHome exposing (FeedTab(..), Model, Msg, init, subscriptions, update, view)

import Api exposing (Cred(..))
import Article.Tag as Tag exposing (Tag)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Loading
import LoadingStatus exposing (Status(..))
import Page.Counter as Counter
import Page.ShareableCounter as SCounter exposing (..)
import Session exposing (Session)
import Table as Tbl
import Task
import Time
import Tuple


type alias Model =
    { session : Session
    , timeZone : Time.Zone
    , feedTab : FeedTab
    , feedPage : Int

    -- Loaded independently from server
    , tags : LoadingStatus.Status (List Tag)
    , tableState : Tbl.State
    , editableTag : Maybe String
    , counterVal : Int
    , counterState : SCounter.State

    --, counterModel : Counter.Model
    --, counterMsg : Counter.Msg
    --    , feed : LoadingStatus.Status Feed.Model
    }


type FeedTab
    = YourFeed Cred
    | GlobalFeed
    | TagFeed Tag


type Msg
    = ClickedTag Tag
    | ClickedTab FeedTab
      --    | ClickedFeedPage Int
      --    | CompletedFeedLoad (Result Http.Error Feed.Model)
    | CompletedTagsLoad (Result Http.Error (List Tag))
    | GotTimeZone Time.Zone
      --    | GotFeedMsg Feed.Msg
    | GotSession Session
    | PassedSlowLoadThreshold
    | SetTableState Tbl.State
    | CellClicked String
    | SharedCounterMessage SCounter.State



--| GotCounterMsg Counter.Msg


sconfig : SCounter.Config Msg
sconfig =
    SCounter.config
        { toMsg = SharedCounterMessage
        }


config : Tbl.Config ( Tag, Bool ) Msg
config =
    Tbl.config
        { toId = \data -> Tag.toString <| Tuple.first data
        , toMsg = SetTableState
        , columns =
            [ Tbl.customColumn { name = "Tag", viewData = \data -> Tag.toString <| Tuple.first data, sorter = Tbl.unsortable }
            , Tbl.veryCustomColumn { name = "Custom Tag", viewData = viewTag, sorter = Tbl.unsortable }
            ]
        }


viewTag : ( Tag, Bool ) -> Tbl.HtmlDetails Msg
viewTag tagWithEditableFlag =
    let
        isEditable =
            Tuple.second tagWithEditableFlag

        tagData =
            Tag.toString (Tuple.first tagWithEditableFlag)
    in
    case isEditable of
        True ->
            Tbl.HtmlDetails [ Html.Events.onClick (CellClicked tagData) ]
                [ Html.span [] [ Html.text "" ]
                , Html.input [ Html.Attributes.value tagData ] []
                ]

        False ->
            Tbl.HtmlDetails [ Html.Events.onClick (CellClicked tagData) ]
                [ Html.span [] [ Html.text "" ]
                , Html.text tagData
                ]


init : Session -> ( Model, Cmd Msg )
init session =
    let
        feedTab =
            case Session.cred session of
                Just cred ->
                    YourFeed cred

                Nothing ->
                    GlobalFeed

        loadTags =
            Http.toTask Tag.list

        ( lcounterModel, lcounterMessage ) =
            Counter.init
    in
    ( { session = session
      , timeZone = Time.utc
      , feedTab = feedTab
      , feedPage = 1
      , tags = LoadingStatus.Loading
      , tableState = Tbl.initialSort "Tag"
      , editableTag = Nothing
      , counterVal = 0
      , counterState = SCounter.initialState 1

      --, counterModel = lcounterModel
      --, counterMsg = lcounterMessage
      --      , feed = LoadingStatus.Loading
      }
    , Cmd.batch
        [ --        fetchFeed session feedTab 1
          --            |> Task.attempt CompletedFeedLoad
          Tag.list
            |> Http.send CompletedTagsLoad
        , Task.perform GotTimeZone Time.here
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedTag tag ->
            let
                feedTab =
                    TagFeed tag
            in
            ( { model | feedTab = feedTab }
            , Cmd.none
              {- , fetchFeed model.session feedTab 1
                 |> Task.attempt CompletedFeedLoad
              -}
            )

        ClickedTab tab ->
            ( { model | feedTab = tab }
            , Cmd.none
              {- , fetchFeed model.session tab 1
                 |> Task.attempt CompletedFeedLoad
              -}
            )

        {- ClickedFeedPage page ->
               ( { model | feedPage = page }
               , fetchFeed model.session model.feedTab page
                   |> Task.andThen (\feed -> Task.map (\_ -> feed) scrollToTop)
                   |> Task.attempt CompletedFeedLoad
               )

           CompletedFeedLoad (Ok feed) ->
               ( { model | feed = Loaded feed }, Cmd.none )

           CompletedFeedLoad (Err error) ->
               ( { model | feed = Failed }, Cmd.none )
        -}
        CompletedTagsLoad (Ok tags) ->
            Debug.log "CompletedTagsLoad Successfully.................."
                ( { model | tags = LoadingStatus.Loaded tags }, Cmd.none )

        CompletedTagsLoad (Err error) ->
            Debug.log "Error loading TAGS.................."
                ( { model | tags = LoadingStatus.Failed }
                , Cmd.none
                )

        {- GotFeedMsg subMsg ->
           case model.feed of
               Loaded feed ->
                   let
                       ( newFeed, subCmd ) =
                           Feed.update (Session.cred model.session) subMsg feed
                   in
                   ( { model | feed = Loaded newFeed }
                   , Cmd.map GotFeedMsg subCmd
                   )

               Loading ->
                   ( model, Log.error )

               LoadingSlowly ->
                   ( model, Log.error )

               Failed ->
                   ( model, Log.error )
        -}
        GotTimeZone tz ->
            ( { model | timeZone = tz }, Cmd.none )

        GotSession session ->
            ( { model | session = session }, Cmd.none )

        PassedSlowLoadThreshold ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                {- feed =
                   case model.feed of
                       Loading ->
                           LoadingSlowly

                       other ->
                           other
                -}
                tags =
                    case model.tags of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
            in
            ( { model | tags = tags }, Cmd.none )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        CellClicked tag ->
            Debug.log ("Tag Clicked " ++ tag)
                ( { model | editableTag = Just tag }, Cmd.none )

        SharedCounterMessage counterState ->
            Debug.log ("Counter state received" ++ Debug.toString counterState)
                ( model, Cmd.none )



{- ( { model | counterState = counterState }, Cmd.none ) -}
--Debug.log ("Counter state received" ++ ((Debug.toString counterState))


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


view : Model -> { title : String, content : Html Msg }
view model =
    let
        acceptableTags =
            case model.tags of
                LoadingStatus.Loaded list ->
                    List.map (\item -> ( item, Tag.toString item == Maybe.withDefault "" model.editableTag )) list

                _ ->
                    []
    in
    { title = "Feed Home"
    , content =
        Html.div []
            [ Html.text ("Feed Home Content goes here" ++ Debug.toString model.tags)
            , Tbl.view config model.tableState acceptableTags
            , SCounter.view sconfig model.counterState

            --, Counter.view model.counterModel
            ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Debug.log
        ("FeedHome module subscriptions"
            ++ Debug.toString model
        )
        Session.changes
        GotSession
        (Session.navKey model.session)
