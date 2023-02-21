module View exposing (..)

import Activities
import Authentication
import Bets.View
import Browser
import Element exposing (paddingXY, spacing)
import Form.View
import RemoteData exposing (RemoteData(..))
import Results.Bets
import Results.Knockouts as Knockouts
import Results.Matches as Matches
import Results.Ranking as Ranking
import Results.Topscorers
import Task
import Types exposing (App(..), Card(..), Credentials(..), DataStatus(..), Flags, InputState(..), Model, Msg(..), Token(..))
import UI.Button
import UI.Screen as Screen
import UI.Style
import Url
import Uuid.Barebones as Uuid


view : Model Msg -> Browser.Document Msg
view model =
    let
        contents =
            let
                contents_ =
                    case model.app of
                        Home ->
                            viewHome model

                        Form ->
                            Form.View.view model

                        BetsDetailsView ->
                            Bets.View.view model

                        Blog ->
                            viewBlog model

                        Login ->
                            Authentication.view model

                        Logoff ->
                            Authentication.view model

                        Bets ->
                            Results.Bets.view model

                        Ranking ->
                            Ranking.view model

                        RankingDetailsView ->
                            Ranking.viewDetails model

                        Results ->
                            Matches.view model

                        EditMatchResult ->
                            Matches.edit model

                        KOResults ->
                            Knockouts.view model

                        TSResults ->
                            Results.Topscorers.view model
            in
            Element.el [ Element.padding 24 ] contents_

        title =
            "Voetbalpool - Corona Editie"

        link app =
            let
                semantics =
                    if app == model.app then
                        UI.Style.Active

                    else
                        UI.Style.Potential

                ( linkUrl, linkText ) =
                    case app of
                        Home ->
                            ( "#home", "home" )

                        Blog ->
                            ( "#blog", "blog" )

                        BetsDetailsView ->
                            ( "#inzendingen", "inzendingen" )

                        Bets ->
                            ( "#inzendingen", "inzendingen" )

                        Form ->
                            ( "#formulier", "formulier" )

                        Ranking ->
                            ( "#stand", "stand" )

                        Results ->
                            ( "#wedstrijden", "wedstrijden" )

                        KOResults ->
                            ( "#knockouts", "knockouts" )

                        TSResults ->
                            ( "#topscorers", "topscorer" )

                        Logoff ->
                            ( "#logout", "logout" )

                        _ ->
                            ( "#home", "home" )
            in
            -- Element.el [] wrapper for wrapperrow alignment misbehaviour
            Element.el [] <| UI.Button.navlink semantics linkUrl linkText

        linkList =
            case model.token of
                RemoteData.Success (Token _) ->
                    [ Home, Ranking, Results, KOResults, TSResults, Blog, Bets, Logoff ]

                _ ->
                    [ Home, Ranking, Results ]

        links =
            Element.wrappedRow [ Element.padding 12, Element.spacing 12 ]
                (List.map link linkList)

        page =
            Element.column
                [ Element.padding 24
                , Element.spacing 24
                , Element.centerX
                ]
                [ links
                , contents
                ]

        body =
            Element.layout (UI.Style.body []) page
    in
    { title = title, body = [ body ] }


getApp : Url.Url -> ( App, Cmd Msg )
getApp url =
    let
        -- emptyFunc =
        --     \_ -> Cmd.none
        ( app, msg ) =
            Maybe.map inspect url.fragment
                |> Maybe.withDefault ( Home, RefreshActivities )

        inspect hash =
            let
                fragment =
                    String.split "/" hash
            in
            case fragment of
                "home" :: _ ->
                    ( Home, RefreshActivities )

                "blog" :: _ ->
                    ( Blog, RefreshActivities )

                "formulier" :: _ ->
                    ( Form, NoOp )

                "inzendingen" :: uuid :: _ ->
                    if Uuid.isValidUuid uuid then
                        ( BetsDetailsView, BetSelected uuid )

                    else
                        ( Ranking, RefreshRanking )

                "inzendingen" :: _ ->
                    ( Bets, FetchBets )

                "stand" :: uuid :: _ ->
                    if Uuid.isValidUuid uuid then
                        ( RankingDetailsView, RetrieveRankingDetails uuid )

                    else
                        ( Ranking, RefreshRanking )

                "stand" :: _ ->
                    ( Ranking, RefreshRanking )

                "wedstrijden" :: "wedstrijd" :: _ ->
                    ( EditMatchResult, NoOp )

                "wedstrijden" :: [] ->
                    ( Results, RefreshResults )

                "knockouts" :: [] ->
                    ( KOResults, RefreshKnockoutsResults )

                "topscorers" :: [] ->
                    ( TSResults, RefreshTopscorerResults )

                "login" :: _ ->
                    ( Login, NoOp )

                "logout" :: _ ->
                    ( Login, Logout )

                _ ->
                    ( Home, RefreshActivities )

        cmd =
            Task.succeed msg
                |> Task.perform identity
    in
    ( app, cmd )


viewHome : Model Msg -> Element.Element Msg
viewHome model =
    Element.column
        [ paddingXY 0 20
        , spacing 30
        , Screen.width model.screen
        , Screen.className "home"
        ]
        [ Activities.viewCommentInput model.activities
        , Activities.view model
        ]


viewBlog : Model Msg -> Element.Element Msg
viewBlog model =
    Element.column
        [ paddingXY 0 20
        , spacing 30
        , Screen.width model.screen
        , Screen.className "blog"
        ]
        [ Activities.viewPostInput model.activities
        , Activities.view model
        ]
