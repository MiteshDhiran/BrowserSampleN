module Main exposing (Model(..), Msg(..), changeRouteTo, getSessionFromModel, init, main, subscriptions, update, updateWith, view)

import Api
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Json.Decode
import Page
import Page.Article as Article
import Page.Blank as Blank
import Page.FeedHome as FeedHome
import Page.Home as Home
import Page.Login as Login
import Page.RuleEditor as RuleEditor
import Route exposing (..)
import Session exposing (Session)
import Url exposing (Url)
import Viewer exposing (Viewer)


type Model
    = Redirect Session
    | Home Home.Model
    | Login Login.Model
    | Article Article.Model
    | FeedHome FeedHome.Model
    | RuleEditor RuleEditor.Model


type Msg
    = Ignored
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotSession Session
    | GotHomeMsg Home.Msg
    | GotLoginMsg Login.Msg
    | GotArticleMsg Article.Msg
    | GotFeedHomeMsg FeedHome.Msg
    | GotRuleEditorMsg RuleEditor.Msg



--init : Url -> Nav.Key -> ( Model, Cmd Msg )
-- flags -> Url.Url -> Navigation.Key -> (model, Cmd msg)


init : Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    Debug.log (Debug.toString url)
        changeRouteTo
        (Route.fromUrl url)
        (Redirect (Session.fromViewer navKey maybeViewer))


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Ignored, _ ) ->
            Debug.log (Debug.toString model)
                ( model, Cmd.none )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            Debug.log ("Nothing" ++ Debug.toString model ++ Debug.toString url)
                                -- If we got a link that didn't include a fragment,
                                -- it's from one of those (href "") attributes that
                                -- we have to include to make the RealWorld CSS work.
                                --
                                -- In an application doing path routing instead of
                                -- fragment-based routing, this entire
                                -- `case url.fragment of` expression this comment
                                -- is inside would be unnecessary.
                                ( model, Cmd.none )

                        Just _ ->
                            Debug.log ("ClickedLink - Just" ++ Debug.toString model ++ Debug.toString url)
                                ( model
                                , Nav.pushUrl (Session.navKey (getSessionFromModel model)) (Url.toString url)
                                )

                Browser.External href ->
                    Debug.log ("Browser.External" ++ Debug.toString model ++ Debug.toString href)
                        ( model
                        , Nav.load href
                        )

        ( ChangedUrl url, _ ) ->
            Debug.log ("ChangedURL" ++ Debug.toString url ++ Debug.toString model)
                changeRouteTo
                (Route.fromUrl url)
                model

        ( GotLoginMsg subMsg, Login login ) ->
            case subMsg of
                _ ->
                    Debug.log ("GotLoginMsg" ++ Debug.toString subMsg ++ Debug.toString login)
                        Login.update
                        subMsg
                        login
                        |> updateWith Login GotLoginMsg model

        ( GotHomeMsg subMsg, Home home ) ->
            Debug.log ("GotHomeMsg" ++ Debug.toString subMsg ++ Debug.toString home)
                Home.update
                subMsg
                home
                |> updateWith Home GotHomeMsg model

        ( GotFeedHomeMsg subMsg, FeedHome feedHome ) ->
            FeedHome.update subMsg feedHome
                |> updateWith FeedHome GotFeedHomeMsg model

        ( GotSession session, Redirect _ ) ->
            Debug.log ("GotSession" ++ Debug.toString session)
                ( Redirect session
                , Route.replaceUrl (Session.navKey session) Route.Home
                )

        ( GotRuleEditorMsg subMsg, RuleEditor ruleEditorModel ) ->
            RuleEditor.update subMsg ruleEditorModel
                |> updateWith RuleEditor GotRuleEditorMsg model

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            Debug.log ("Discarded update message" ++ Debug.toString model)
                ( model, Cmd.none )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            getSessionFromModel model
    in
    case maybeRoute of
        Nothing ->
            ( model, Cmd.none )

        Just Route.Root ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Home )

        Just Route.Login ->
            Login.init session
                |> updateWith Login GotLoginMsg model

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg model

        Just (Route.Article slug) ->
            Article.init session
                |> updateWith Article GotArticleMsg model

        Just Route.FeedHome ->
            FeedHome.init session
                |> updateWith FeedHome GotFeedHomeMsg model

        Just Route.RuleEditor ->
            RuleEditor.init session
                |> updateWith RuleEditor GotRuleEditorMsg model


getSessionFromModel : Model -> Session
getSessionFromModel model =
    case model of
        Redirect session ->
            session

        Home homemodel ->
            homemodel.session

        Login loginmodel ->
            loginmodel.session

        Article artcilemodel ->
            artcilemodel.session

        FeedHome feedhomeModel ->
            feedhomeModel.session

        RuleEditor ruleeditorModel ->
            ruleeditorModel.session


view : Model -> Browser.Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view (getSessionFromModel model) page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ ->
            viewPage Page.Home (\_ -> Ignored) Blank.view

        Home home ->
            viewPage Page.Home GotHomeMsg (Home.view home)

        Login login ->
            viewPage Page.Login GotLoginMsg (Login.view login)

        Article article ->
            viewPage Page.Article GotArticleMsg (Article.view article)

        FeedHome feedHome ->
            viewPage Page.FeedHome GotFeedHomeMsg (FeedHome.view feedHome)

        RuleEditor ruleeditor ->
            viewPage Page.RuleEditor GotRuleEditorMsg (RuleEditor.view ruleeditor)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Redirect _ ->
            --changes : (Session -> msg) -> Nav.Key -> Sub msg
            --Api.viewerChanges (\maybeViewer -> toMsg (fromViewer key maybeViewer)) Viewer.decoder
            {- viewerChanges : (Maybe viewer -> msg) -> Decoder (Cred -> viewer) -> Sub msg
               viewerChanges toMsg decoder =
                   onStoreChange (\value -> toMsg (decodeFromChange decoder value))
            -}
            {- port onStoreChange : (Value -> msg) -> Sub msg -}
            Session.changes GotSession (Session.navKey (getSessionFromModel model))

        Home home ->
            Sub.map GotHomeMsg (Home.subscriptions home)

        Login login ->
            Debug.log "Main module Subscription calling GotLoginMsg"
                Sub.map
                GotLoginMsg
                (Login.subscriptions login)

        Article article ->
            Sub.map GotArticleMsg (Article.subscriptions article)

        FeedHome feedHome ->
            Sub.map GotFeedHomeMsg (FeedHome.subscriptions feedHome)

        RuleEditor ruleeditor ->
            Sub.none



{- main =
   Browser.application
       { init = init
       , onUrlChange = ChangedUrl
       , onUrlRequest = ClickedLink
       , subscriptions = subscriptions
       , update = update
       , view = view
       }
-}


main : Program Json.Decode.Value Model Msg
main =
    Api.application Viewer.decoder
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
