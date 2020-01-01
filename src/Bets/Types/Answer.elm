module Bets.Types.Answer exposing
    ( decode
    , encode
    , encodeAnswer
    , isComplete
    , summary
    )

import Bets.Types exposing (Answer, AnswerID, AnswerT(..))
import Bets.Types.Bracket
import Bets.Types.Group
import Bets.Types.Match
import Bets.Types.Participant
import Bets.Types.Points
import Bets.Types.Score
import Bets.Types.Topscorer
import Json.Decode exposing (Decoder, andThen, field, map2, map4, maybe)
import Json.Encode


isComplete : Answer -> Bool
isComplete ( _, answer ) =
    case answer of
        AnswerGroupMatch _ _ mScore _ ->
            Bets.Types.Score.isComplete mScore

        AnswerBracket bracket _ ->
            Bets.Types.Bracket.isComplete bracket

        AnswerTopscorer topscorer _ ->
            Bets.Types.Topscorer.isComplete topscorer

        AnswerParticipant participant ->
            Bets.Types.Participant.isComplete participant


summary : Answer -> String
summary ( _, answer ) =
    case answer of
        AnswerGroupMatch group _ _ _ ->
            "Wedstrijden " ++ Bets.Types.Group.toString group

        -- should be ++ match id
        AnswerBracket _ _ ->
            "Schema"

        AnswerTopscorer _ _ ->
            "Topscorer"

        AnswerParticipant _ ->
            "Over jou"


encodeAnswer : Answer -> ( AnswerID, Json.Encode.Value )
encodeAnswer ( answerId, answerT ) =
    ( answerId, encodeAnswerT answerT )


encode : Answer -> Json.Encode.Value
encode ( answerID, answerT ) =
    Json.Encode.object
        [ ( "id", Json.Encode.string answerID )
        , ( "answer", encodeAnswerT answerT )
        ]


encodeAnswerT : AnswerT -> Json.Encode.Value
encodeAnswerT answerT =
    case answerT of
        AnswerGroupMatch group match mScore points ->
            Json.Encode.object
                [ ( "answerType", Json.Encode.string "answer-group-match" )
                , ( "group", Bets.Types.Group.encode group )
                , ( "match", Bets.Types.Match.encode match )
                , ( "score", Bets.Types.Score.encodeMaybe mScore )
                , ( "points", Bets.Types.Points.encode points )
                ]

        AnswerBracket bracket points ->
            Json.Encode.object
                [ ( "answerType", Json.Encode.string "answer-bracket" )
                , ( "bracket", Bets.Types.Bracket.encode bracket )
                , ( "points", Bets.Types.Points.encode points )
                ]

        AnswerTopscorer topscorer points ->
            Json.Encode.object
                [ ( "answerType", Json.Encode.string "answer-topscorer" )
                , ( "topscorer", Bets.Types.Topscorer.encode topscorer )
                , ( "points", Bets.Types.Points.encode points )
                ]

        AnswerParticipant participant ->
            Json.Encode.object
                [ ( "answerType", Json.Encode.string "answer-participant" )
                , ( "participant", Bets.Types.Participant.encode participant )
                ]


decode : Decoder AnswerT
decode =
    field "answerType" Json.Decode.string
        |> andThen decodeAnswerTDetails


decodeAnswerTDetails : String -> Decoder AnswerT
decodeAnswerTDetails s =
    case s of
        "answer-group-match" ->
            Json.Decode.map4 AnswerGroupMatch
                (field "group" Bets.Types.Group.decode)
                (field "match" Bets.Types.Match.decode)
                (field "score" (maybe Bets.Types.Score.decode))
                (field "points" Bets.Types.Points.decode)

        "answer-bracket" ->
            Json.Decode.map2 AnswerBracket
                (field "bracket" Bets.Types.Bracket.decode)
                (field "points" Bets.Types.Points.decode)

        "answer-topscorer" ->
            Json.Decode.map2 AnswerTopscorer
                (field "topscorer" Bets.Types.Topscorer.decode)
                (field "points" Bets.Types.Points.decode)

        "answer-participant" ->
            Json.Decode.map AnswerParticipant
                (field "participant" Bets.Types.Participant.decode)

        _ ->
            Json.Decode.fail "unknown type of question"
