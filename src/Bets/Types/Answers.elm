module Bets.Types.Answers exposing
    ( cleanThirds
    , decode
    , encode
    , points
    , setQualifier
    , setScore
    , setTopscorer
    , setWinner
    )

import Bets.Types exposing (Answer(..), Answers, Group, MatchID, Points, Qualifier, Score, Slot, Topscorer, Winner)
import Bets.Types.Answer.Bracket as Br
import Bets.Types.Answer.GroupMatches as Gm
import Bets.Types.Answer.Topscorer as Ts
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode



-- EXPOSED


setTopscorer : Answers -> Topscorer -> Answers
setTopscorer answers ts =
    { answers | topscorer = Ts.set answers.topscorer ts }


setQualifier : Answers -> Slot -> Group -> Qualifier -> Answers
setQualifier answers slot grp winner =
    { answers | bracket = Br.setQualifier answers.bracket slot grp winner }


setWinner : Answers -> Slot -> Winner -> Answers
setWinner answers slot winner =
    { answers | bracket = Br.setWinner answers.bracket slot winner }


cleanThirds : Answers -> Group -> Answers
cleanThirds answers grp =
    { answers | bracket = Br.cleanThirds answers.bracket grp }


setScore : Answers -> MatchID -> Score -> Answers
setScore answers matchID score =
    { answers | matches = Gm.setScore answers.matches matchID score }


points : Answer a -> Points
points (Answer _ pts) =
    pts


encode : Answers -> Json.Encode.Value
encode answers =
    Json.Encode.object
        [ ( "matches", Gm.encode answers.matches )
        , ( "bracket", Br.encode answers.bracket )
        , ( "topscorer", Ts.encode answers.topscorer )
        ]


decode : Decoder Answers
decode =
    Decode.succeed Answers
        |> required "matches" Gm.decode
        |> required "bracket" Br.decode
        |> required "topscorer" Ts.decode
