module Form.View exposing (view)

import Bets.Bet
import Bets.Types.Group as Group
import Browser
import Element exposing (padding, paddingXY, spacing)
import Form.Bracket
import Form.GroupMatches
import Form.Info
import Form.Participant
import Form.Submit
import Form.Topscorer
import Form.Types exposing (Card(..), Model, Msg(..))
import UI.Button
import UI.Style



-- View


view : Model Msg -> Browser.Document Msg
view model =
    let
        getCard =
            List.drop model.idx model.cards
                |> List.head

        makeCard mCard =
            case mCard of
                Just card_ ->
                    viewCard model model.idx card_

                Nothing ->
                    Element.none

        card =
            getCard
                |> makeCard

        body =
            viewCardChrome model card model.idx
                |> Element.layout (UI.Style.body [])

        title =
            "Euro 2020"
    in
    { title = title, body = [ body ] }


viewCard : Model Msg -> Int -> Card -> Element.Element Msg
viewCard model i card =
    case card of
        IntroCard intro ->
            Element.map InfoMsg (Form.Info.view intro)

        -- QuestionCard qModel ->
        --     Element.map (Answered i) (Form.Question.view model.bet qModel)
        -- QuestionSetCard qsModel ->
        --     Element.map (QuestionSetMsg i) (Form.QuestionSet.view qsModel model.bet)
        GroupMatchesCard groupMatchesState ->
            Element.map (GroupMatchMsg groupMatchesState.group) (Form.GroupMatches.view model.bet groupMatchesState)

        BracketCard bracketState ->
            Element.map BracketMsg (Form.Bracket.view model.bet bracketState)

        TopscorerCard ->
            Element.map TopscorerMsg (Form.Topscorer.view model.bet)

        ParticipantCard ->
            Element.map ParticipantMsg (Form.Participant.view model.bet)

        SubmitCard ->
            let
                submittable =
                    Bets.Bet.isComplete model.bet
            in
            Form.Submit.view model submittable


viewPill : Model Msg -> Int -> ( Int, Card ) -> Element.Element Msg
viewPill model idx ( i, card ) =
    let
        semantics =
            case card of
                IntroCard _ ->
                    UI.Style.Perhaps

                GroupMatchesCard state ->
                    mkpillModel (Form.GroupMatches.isComplete state.group model.bet) (i == idx)

                BracketCard _ ->
                    mkpillModel (Form.Bracket.isComplete model.bet) (i == idx)

                TopscorerCard ->
                    mkpillModel (Form.Topscorer.isComplete model.bet) (i == idx)

                ParticipantCard ->
                    mkpillModel (Form.Participant.isComplete model.bet) (i == idx)

                SubmitCard ->
                    UI.Style.Perhaps

        mkpillModel complete current =
            case ( complete, current ) of
                ( True, True ) ->
                    UI.Style.Selected

                ( True, False ) ->
                    UI.Style.Active

                ( False, True ) ->
                    UI.Style.Right

                ( False, False ) ->
                    UI.Style.Wrong

        msg =
            NavigateTo i

        contents =
            case card of
                IntroCard _ ->
                    "Start"

                GroupMatchesCard state ->
                    "Wedstrijden " ++ Group.toString state.group

                BracketCard state ->
                    "Schema"

                TopscorerCard ->
                    "Topscorer"

                ParticipantCard ->
                    "Over jou"

                SubmitCard ->
                    "Insturen"
    in
    UI.Button.pill semantics
        msg
        contents


viewCardChrome : Model Msg -> Element.Element Msg -> Int -> Element.Element Msg
viewCardChrome model card i =
    let
        next =
            Basics.min (i + 1) (List.length model.cards - 1)

        prev =
            Basics.max (i - 1) 0

        pills =
            List.map (viewPill model i) (List.indexedMap (\a b -> ( a, b )) model.cards)

        prevPill =
            UI.Button.pill UI.Style.Irrelevant (NavigateTo prev) "vorige"

        nextPill =
            UI.Button.pill UI.Style.Irrelevant (NavigateTo next) "volgende"

        pillsPlus =
            prevPill
                :: List.append pills [ nextPill ]
                |> Element.wrappedRow (UI.Style.none [ spacing 7, padding 0 ])
    in
    Element.column (UI.Style.none [ spacing 20, paddingXY 70 20 ])
        [ pillsPlus
        , card
        ]
