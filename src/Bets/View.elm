module Bets.View exposing (view, viewBet)

import Bets.Types exposing (..)
import Bets.Types.Answers as Answers
import Bets.Types.Bracket as B
import Bets.Types.Score as S
import Bets.Types.StringField as StringField
import Bets.View.Bracket
import Element exposing (Element, centerX, centerY, height, padding, paddingEach, paddingXY, px, spacing, spacingXY, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import RemoteData exposing (RemoteData(..))
import Types exposing (Model, Msg(..))
import UI.Button
import UI.Color as Color
import UI.Font exposing (scaled)
import UI.Match
import UI.Screen as Screen exposing (Device(..))
import UI.Style exposing (ButtonSemantics(..))
import UI.Team
import UI.Text


view : Model Msg -> Element Msg
view model =
    case model.savedBet of
        NotAsked ->
            Element.text "Aan het ophalen."

        Loading ->
            Element.text "Aan het ophalen..."

        Failure _ ->
            -- let
            --     t =
            --         Debug.log "error! " err
            -- in
            UI.Text.error "Oeps. Daar ging iets niet goed."

        Success bet ->
            viewBet bet model.screen


viewBet : Bet -> Screen.Size -> Element.Element Msg
viewBet bet screenSize =
    let
        w =
            Screen.width screenSize

        device =
            Screen.device screenSize

        pointsForAnswer mPts =
            Maybe.map String.fromInt mPts
                |> Maybe.map (\p -> " (" ++ p ++ " punten)")
                |> Maybe.withDefault ""

        matchesTotal =
            List.filterMap (Tuple.second >> Answers.points) bet.answers.matches
                |> List.sum
                |> Just
                |> pointsForAnswer

        bracketTotal =
            pointsForAnswer <| Answers.points bet.answers.bracket

        topscorerTotal =
            pointsForAnswer <| Answers.points bet.answers.topscorer
    in
    Element.column
        [ spacing 40, w ]
        [ displayParticipant bet.participant
        , UI.Text.displayHeader ("De Topscorer" ++ topscorerTotal)
        , displayTopscorer bet.answers.topscorer
        , topscorerIntro
        , UI.Text.displayHeader ("Het Schema" ++ bracketTotal)
        , displayBracket screenSize bet
        , UI.Text.displayHeader ("De wedstrijden" ++ matchesTotal)
        , displayMatches bet.answers.matches
        , matchesIntro
        ]


matchesIntro : Element.Element Msg
matchesIntro =
    let
        introtext =
            """Voor iedere wedstrijd die je helemaal goed hebt voorspeld krijg je 3 punten.
            Heb je enkel de toto goed, krijg je 1 punt. En anders niets.
            """
    in
    Element.paragraph [] [ UI.Text.simpleText introtext ]


topscorerIntro : Element.Element Msg
topscorerIntro =
    let
        introtext =
            """De topscorer levert 9 punten op, mits goed voorspeld natuurlijk.
            """
    in
    Element.paragraph [] [ UI.Text.simpleText introtext ]


displayMatches : List ( MatchID, AnswerGroupMatch ) -> Element.Element Msg
displayMatches answers =
    let
        sortedMatches =
            List.sortBy Tuple.first answers
    in
    Element.wrappedRow
        [ padding 10, spacingXY 20 40, centerX ]
        (List.map displayMatch sortedMatches)


displayMatch : ( MatchID, AnswerGroupMatch ) -> Element.Element Msg
displayMatch ( _, Answer groupMatch pts ) =
    let
        semantics =
            case pts of
                Just 3 ->
                    UI.Style.Right

                Just 1 ->
                    UI.Style.Active

                Just 0 ->
                    UI.Style.Wrong

                _ ->
                    UI.Style.Perhaps

        handler =
            onClick NoOp

        disp (GroupMatch _ match mScore) =
            UI.Match.display match mScore handler semantics
    in
    disp groupMatch


displayBracket : Screen.Size -> Bet -> Element.Element Msg
displayBracket screen bet =
    let
        br =
            bet.answers.bracket

        introtext =
            """Dit is het schema voor de tweede ronde en verder. In het midden staat de finale en de kampioen,
         daarboven en onder de ronden die daaraan voorafgaan. Voor ieder team dat je juist hebt in de tweede
         ronde krijg je 1 punt. Voor de juiste kwartfinalisten krijg je 4 punten. Halve finalisten leveren 7
         punten op, finalisten 10 punten en de kampioen 13 punten."""

        introduction =
            Element.paragraph [ paddingEach { top = 0, right = 0, bottom = 32, left = 0 } ] [ UI.Text.simpleText introtext ]

        rings (Answer brkt _) =
            Bets.View.Bracket.viewRings bet brkt screen
    in
    Element.column
        [ spacing 20 ]
        [ rings br
        , introduction
        ]


displayTopscorer : Answer Topscorer -> Element.Element Msg
displayTopscorer (Answer ts points) =
    let
        nameColor =
            case points of
                Nothing ->
                    UI.Style.Potential

                Just 9 ->
                    UI.Style.Right

                Just _ ->
                    UI.Style.Wrong

        tsName mTs =
            Maybe.map (UI.Button.pill nameColor Types.NoOp) mTs
                |> Maybe.withDefault (error "no topscorer")

        teamBadge =
            UI.Button.maybeTeamBadgeSmall Potential (Tuple.second ts)
                |> badge
    in
    Element.row
        [ spacing 20 ]
        [ teamBadge
        , tsName (Tuple.first ts)
        ]


badge el =
    Element.el
        [ height (px 100)
        , width (px 80)
        , Background.color Color.panel
        , Font.color Color.primaryText
        , Font.size (scaled 1)
        , Font.center
        , centerY
        , Element.pointer
        , UI.Font.match
        , Border.rounded 10
        , spacing 0
        ]
        el


badgeVerySmall el =
    Element.el
        [ height (px 50)
        , width (px 40)
        , Background.color Color.panel
        , Font.color Color.primaryText
        , Font.size (scaled 1)
        , Font.center
        , centerY
        , Element.pointer
        , UI.Font.match
        , Border.rounded 3
        , spacing 0
        ]
        el


error : String -> Element.Element msg
error text =
    Element.row [] [ Element.text text ]


displayParticipant : Participant -> Element.Element msg
displayParticipant participant =
    let
        h part =
            StringField.value part.name
                |> UI.Text.displayHeader

        residenceText =
            (++) "uit "

        p part =
            StringField.value part.residence
                |> UI.Text.simpleText
    in
    Element.column
        [ spacing 20, centerY ]
        [ h participant
        , p participant
        ]
