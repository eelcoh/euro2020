module Bets.Bet exposing
    ( decode
    , encode
    , findAllGroupMatchAnswers
    , findGroupMatchAnswers
    , getAnswer
    , setMatchScore
    , setParticipant
    , setQualifier
    , setTopscorer
    , setWinner
    )

import Bets.Json.Encode exposing (mIntEnc, mStrEnc)
import Bets.Types exposing (Answer, AnswerID, Answers, Bet, Group, Participant, Qualifier, Score, Slot, Topscorer, Winner)
import Bets.Types.Answers
import Json.Decode exposing (Decoder, field, maybe)
import Json.Encode


getAnswer : Bet -> AnswerID -> Maybe Answer
getAnswer bet answerId =
    Bets.Types.Answers.getAnswer bet.answers answerId



-- candidates : Bet -> Answer -> Candidates
-- candidates bet answer =
--     Bets.Types.Candidates.candidates bet.answers answer


findAllGroupMatchAnswers : Bet -> Answers
findAllGroupMatchAnswers bet =
    Bets.Types.Answers.findAllGroupMatchAnswers bet.answers


findGroupMatchAnswers : Group -> Bet -> Answers
findGroupMatchAnswers group bet =
    Bets.Types.Answers.findGroupMatchAnswers group bet.answers


newBet : Bet -> Answers -> Bet
newBet bet newAnswers =
    { bet | answers = newAnswers }


setWinner : Bet -> Answer -> Slot -> Winner -> Bet
setWinner bet answer slot winner =
    Bets.Types.Answers.setWinner bet.answers answer slot winner
        |> newBet bet


setQualifier : Bet -> Answer -> Slot -> Qualifier -> Bet
setQualifier bet answer slot qualifier =
    Bets.Types.Answers.setQualifier bet.answers answer slot qualifier
        |> newBet bet



-- setTeam : Bet -> Answer -> Group -> Team -> Bet
-- setTeam bet answer group team =
--     Bets.Types.Answers.setTeam bet.answers answer group team
--         |> newBet bet


setMatchScore : Bet -> Answer -> Score -> Bet
setMatchScore bet answer score =
    Bets.Types.Answers.setMatchScore bet.answers answer score
        |> newBet bet


setParticipant : Bet -> Answer -> Participant -> Bet
setParticipant bet answer participant =
    Bets.Types.Answers.setParticipant bet.answers answer participant
        |> newBet bet


setTopscorer : Bet -> Answer -> Topscorer -> Bet
setTopscorer bet answer topscorer =
    Bets.Types.Answers.setTopscorer bet.answers answer topscorer
        |> newBet bet


encode : Bet -> Json.Encode.Value
encode bet =
    let
        betObject =
            Json.Encode.object
                [ ( "answers", Bets.Types.Answers.encode bet.answers )
                , ( "betId", mIntEnc bet.betId )
                , ( "uuid", mStrEnc bet.uuid )
                , ( "active", Json.Encode.bool bet.active )
                ]
    in
    Json.Encode.object
        [ ( "bet", betObject ) ]


type alias IncomingBet =
    { bet : Bet }


decode : Decoder Bet
decode =
    Json.Decode.map
        (\x -> x.bet)
        decodeIncoming


decodeIncoming : Decoder IncomingBet
decodeIncoming =
    Json.Decode.map
        IncomingBet
        (field "bet" decodeBet)


decodeBet : Decoder Bet
decodeBet =
    Json.Decode.map4 Bet
        (field "answers" Bets.Types.Answers.decode)
        (field "betId" (maybe Json.Decode.int))
        (field "uuid" (maybe Json.Decode.string))
        (field "active" Json.Decode.bool)
