module Results.Topscorers exposing (..)

import Bets.Types exposing (HasQualified(..), Topscorer)
import Bets.Types.HasQualified
import Bets.Types.Topscorer as Topscorer
import Element exposing (Element, padding, spacing, spacingXY)
import Element.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field)
import Json.Encode
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Web exposing (defaultConfig)
import Types exposing (Access(..), Activity(..), DataStatus(..), Model, Msg(..), Qualified(..), Token(..), TopscorerResults)
import UI.Button
import UI.Style


fetchTopscorerResults : Cmd Msg
fetchTopscorerResults =
    Web.get "/bets/results/topscorer/" FetchedTopscorerResults decode


inititaliseTopscorerResults : Token -> Cmd Msg
inititaliseTopscorerResults (Token token) =
    let
        bearer =
            "Bearer " ++ token

        header =
            Http.header "Authorization" bearer

        config =
            { defaultConfig
                | headers = [ header ]
            }

        url =
            "/bets/results/topscorer/initial/"

        json =
            Json.Encode.object []
    in
    Web.postWithConfig config url FetchedTopscorerResults decode json


storeTopscorerResults : Token -> TopscorerResults -> Cmd Msg
storeTopscorerResults (Token token) results =
    let
        bearer =
            "Bearer " ++ token

        header =
            Http.header "Authorization" bearer

        config =
            { defaultConfig
                | headers = [ header ]
            }

        url =
            "/bets/results/topscorer/"

        json =
            encode results
    in
    Web.postWithConfig config url StoredTopscorerResults decode json



-- update


update : HasQualified -> Topscorer -> WebData TopscorerResults -> WebData TopscorerResults
update qualified topscorer results =
    case results of
        Success results_ ->
            updateTopscorerResults qualified topscorer results_
                |> RemoteData.succeed

        _ ->
            results


updateTopscorerResults : HasQualified -> Topscorer -> TopscorerResults -> TopscorerResults
updateTopscorerResults qualified topscorer results =
    let
        topscorerResults =
            List.map (updateTopscorer qualified topscorer) results.topscorers
    in
    { results | topscorers = topscorerResults }


updateTopscorer : HasQualified -> Topscorer -> ( HasQualified, Topscorer ) -> ( HasQualified, Topscorer )
updateTopscorer qualified ts ( hasQ, topscorer ) =
    if Topscorer.equal ts topscorer then
        ( qualified, topscorer )

    else
        ( hasQ, topscorer )



-- View


view : Model Msg -> Element.Element Msg
view model =
    let
        auth =
            case model.token of
                Success _ ->
                    Authorised

                _ ->
                    Unauthorised

        items =
            case ( auth, model.topscorerResults ) of
                ( Authorised, Fresh (Success results) ) ->
                    [ UI.Button.pill UI.Style.Inactive UpdateTopscorerResults "Update"
                    , viewTopscorerResults auth results
                    , UI.Button.pill UI.Style.Potential InitialiseTopscorerResults "Initialiseer"
                    ]

                ( Authorised, Filthy (Success results) ) ->
                    [ UI.Button.pill UI.Style.Active UpdateTopscorerResults "Update"
                    , viewTopscorerResults auth results
                    , UI.Button.pill UI.Style.Potential InitialiseTopscorerResults "Initialiseer"
                    ]

                ( Authorised, _ ) ->
                    [ UI.Button.pill UI.Style.Inactive UpdateTopscorerResults "Update"
                    , Element.text "Nog niet bekend"
                    , UI.Button.pill UI.Style.Potential InitialiseTopscorerResults "Initialiseer"
                    ]

                ( Unauthorised, Fresh (Success results) ) ->
                    [ viewTopscorerResults auth results ]

                ( _, Fresh (Failure err) ) ->
                    [ Element.text "(toString err)" ]

                ( _, _ ) ->
                    [ Element.text "..." ]
    in
    Element.column
        [ spacingXY 0 14 ]
        items



-- {teams : List (TeamId, { team: Team, roundsQualified : List (Round, HasQualified)})}


viewTopscorerResults : Access -> TopscorerResults -> Element.Element Msg
viewTopscorerResults _ results =
    Element.wrappedRow
        [ spacing 20 ]
        (List.map viewTopscorer results.topscorers)


viewTopscorer : ( HasQualified, Topscorer ) -> Element.Element Msg
viewTopscorer ( hasQualified, topscorer ) =
    let
        msg =
            case hasQualified of
                In ->
                    ChangeTopscorerResults TBD topscorer

                Out ->
                    ChangeTopscorerResults In topscorer

                TBD ->
                    ChangeTopscorerResults Out topscorer

        teamBadge =
            Topscorer.getTeam topscorer
                |> Maybe.map (UI.Button.teamButton semantics msg)
                |> Maybe.withDefault Element.none

        name =
            Maybe.withDefault "OEPS" <| Topscorer.getPlayer topscorer

        semantics =
            case hasQualified of
                TBD ->
                    UI.Style.Potential

                In ->
                    UI.Style.Right

                Out ->
                    UI.Style.Wrong
    in
    Element.row
        [ spacing 20, padding 10, onClick msg ]
        [ teamBadge
        , UI.Button.pill semantics msg name
        ]


encode : TopscorerResults -> Json.Encode.Value
encode results =
    Json.Encode.object
        [ ( "topscorers", Json.Encode.list encodeTopscorerResult results.topscorers )
        ]


encodeTopscorerResult : ( HasQualified, Topscorer ) -> Json.Encode.Value
encodeTopscorerResult ( hasQualified, topscorer ) =
    let
        ts =
            Topscorer.encode topscorer

        hq =
            Bets.Types.HasQualified.encode hasQualified
    in
    Json.Encode.object
        [ ( "topscorer", ts )
        , ( "hasQualified", hq )
        ]


decode : Decoder TopscorerResults
decode =
    Json.Decode.map TopscorerResults
        (field "topscorers" (Json.Decode.list decodeTopscorerResult))


decodeTopscorerResult : Decoder ( HasQualified, Topscorer )
decodeTopscorerResult =
    Json.Decode.map2 Tuple.pair
        (field "hasQualified" Bets.Types.HasQualified.decode)
        (field "topscorer" Topscorer.decode)
