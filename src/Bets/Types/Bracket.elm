module Bets.Types.Bracket exposing
    ( candidatesForTeamNode
    , decode
    , decodeWinner
    , display
    , encode
    , get
    , getQualifiers
    , getSlotsAndCandidates
    , isComplete
    , proceed
    , proceedAway
    , proceedHome
    , qualifier
    , set
    , setBulk
    , unsetQualifier
    , winner
    )

import Bets.Types exposing (Bracket(..), Candidate, CurrentSlot(..), Group, HasQualified(..), Qualifier, Selection, Slot, Team, Winner(..))
import Bets.Types.Candidate as C
import Bets.Types.Round as R
import Bets.Types.Team as T
import Dict
import Json.Decode exposing (Decoder, fail, field, lazy, maybe)
import Json.Encode
import Maybe.Extra as M


reset : Bracket -> Qualifier -> Bracket
reset newBracket prevWinner =
    case newBracket of
        MatchNode slot w home away rnd hasQ ->
            let
                currentWinner =
                    winner newBracket

                newWinner =
                    if currentWinner == prevWinner then
                        w

                    else
                        None
            in
            -- should really reassess hasQ, but not necessary now since it can only be TBD
            MatchNode slot newWinner home away rnd hasQ

        TeamNode slot pos qual hasQ ->
            TeamNode slot pos qual hasQ


proceedHome : Bracket -> Slot -> Bracket
proceedHome bracket slot =
    proceed bracket slot HomeTeam


proceedAway : Bracket -> Slot -> Bracket
proceedAway bracket slot =
    proceed bracket slot AwayTeam


proceed : Bracket -> Slot -> Winner -> Bracket
proceed bracket slot wnnr =
    case bracket of
        MatchNode s w home away rnd hasQ ->
            if s == slot then
                -- should really reassess hasQ, but not necessary now since it can only be TBD
                MatchNode slot wnnr home away rnd hasQ

            else
                let
                    newLeft =
                        proceed home slot wnnr

                    newRight =
                        proceed away slot wnnr

                    currentWinner =
                        winner bracket

                    -- should really reassess hasQ, but not necessary now since it can only be TBD
                    newBracket =
                        MatchNode s w newLeft newRight rnd hasQ
                in
                reset newBracket currentWinner

        _ ->
            bracket


winner : Bracket -> Qualifier
winner bracket =
    case bracket of
        MatchNode slot w home away _ _ ->
            case w of
                HomeTeam ->
                    winner home

                AwayTeam ->
                    winner away

                None ->
                    Nothing

        TeamNode slot pos qual hasQ ->
            qual


set : Bracket -> Slot -> Qualifier -> Bracket
set bracket slot qual =
    case bracket of
        MatchNode s w home away round hasQ ->
            let
                newHome =
                    set home slot qual

                newAway =
                    set away slot qual

                newBracket =
                    MatchNode s w newHome newAway round hasQ

                currentWinner =
                    winner bracket
            in
            reset newBracket currentWinner

        TeamNode s pos mt hasQ ->
            if s == slot then
                TeamNode slot pos qual hasQ

            else
                bracket


setBulk : Bracket -> List ( Slot, Qualifier ) -> Bracket
setBulk bracket slots =
    let
        newSet ( slot, qual ) brkt =
            set brkt slot qual
    in
    List.foldl newSet bracket slots


unsetQualifier : Bracket -> Qualifier -> Bracket
unsetQualifier bracket qual =
    case bracket of
        MatchNode s w home away round hasQ ->
            let
                newHome =
                    unsetQualifier home qual

                newAway =
                    unsetQualifier away qual

                newBracket =
                    MatchNode s w newHome newAway round hasQ

                currentWinner =
                    winner bracket
            in
            reset newBracket currentWinner

        TeamNode slot pos mt hasQ ->
            if mt == qual then
                TeamNode slot pos Nothing hasQ

            else
                bracket


qualifier : Bracket -> Qualifier
qualifier bracket =
    case bracket of
        MatchNode s w home away round hasQ ->
            case w of
                HomeTeam ->
                    qualifier home

                AwayTeam ->
                    qualifier away

                None ->
                    Nothing

        TeamNode s p q _ ->
            q


get : Bracket -> Slot -> Maybe Bracket
get brkt slot =
    case brkt of
        MatchNode s w home away round hasQ ->
            if s == slot then
                Just brkt

            else
                let
                    oneOf =
                        M.values >> List.head
                in
                oneOf [ get away slot, get home slot ]

        TeamNode s p q hasQ ->
            if s == slot then
                Just brkt

            else
                Nothing


getQualifiers : Bracket -> List ( Slot, Qualifier )
getQualifiers brkt =
    case brkt of
        MatchNode s w home away round hasQ ->
            List.concat [ getQualifiers away, getQualifiers home ]

        TeamNode s p q hasQ ->
            [ ( s, q ) ]


display : Bracket -> String
display bracket =
    T.mdisplay (qualifier bracket)


isComplete : Bracket -> Bool
isComplete brkt =
    case brkt of
        TeamNode slot pos qual hasQ ->
            M.isJust qual

        MatchNode slot w home away round hasQ ->
            case w of
                None ->
                    False

                _ ->
                    isComplete home && isComplete away


candidatesForTeamNode : Bracket -> Candidate -> Slot -> List Selection
candidatesForTeamNode brkt position slot =
    let
        candidates =
            C.get position

        -- returns Dict (TeamID, Slot)
        qualifiers =
            getQualifiers brkt
                |> List.map justify
                |> M.values
                |> Dict.fromList

        justify ( a, m ) =
            case m of
                Just b ->
                    Just ( b.teamID, a )

                Nothing ->
                    Nothing

        assess : ( Group, Team ) -> Selection
        assess ( g, t ) =
            case Dict.get t.teamID qualifiers of
                Just s ->
                    if s == slot then
                        Selection ThisSlot g t

                    else
                        Selection (OtherSlot s) g t

                Nothing ->
                    Selection NoSlot g t
    in
    List.map assess candidates


getSlotsAndCandidates : Bracket -> List ( Slot, Candidate )
getSlotsAndCandidates bracket =
    case bracket of
        MatchNode _ _ home away _ _ ->
            List.concatMap getSlotsAndCandidates [ home, away ]

        TeamNode slot candidate _ _ ->
            [ ( slot, candidate ) ]



-- JSON


encodeWinner : Winner -> Json.Encode.Value
encodeWinner wnnr =
    case wnnr of
        HomeTeam ->
            Json.Encode.string "HomeTeam"

        AwayTeam ->
            Json.Encode.string "AwayTeam"

        None ->
            Json.Encode.null


encode : Bracket -> Json.Encode.Value
encode bracket =
    case bracket of
        TeamNode slot pos qual hasQ ->
            Json.Encode.object
                [ ( "node", Json.Encode.string "team" )
                , ( "slot", Json.Encode.string slot )
                , ( "candidates", C.encode pos )
                , ( "qualifier", T.encodeMaybe qual )
                , ( "hasQualified", encodeHasQualified hasQ )
                ]

        MatchNode slot wnnr home away round hasQ ->
            Json.Encode.object
                [ ( "node", Json.Encode.string "match" )
                , ( "slot", Json.Encode.string slot )
                , ( "winner", encodeWinner wnnr )
                , ( "home", encode home )
                , ( "away", encode away )
                , ( "round", R.encode round )
                , ( "hasQualified", encodeHasQualified hasQ )
                ]


decode : Decoder Bracket
decode =
    field "node" Json.Decode.string
        |> Json.Decode.andThen
            (\node ->
                case node of
                    "team" ->
                        Json.Decode.map4 TeamNode
                            (field "slot" Json.Decode.string)
                            (field "candidates" C.decode)
                            (field "qualifier" (maybe T.decode))
                            (field "hasQualified" decodeHasQualified)

                    "match" ->
                        Json.Decode.map6 MatchNode
                            (field "slot" Json.Decode.string)
                            (field "winner" (maybe Json.Decode.string |> Json.Decode.andThen decodeWinner))
                            (field "home" (lazy (\_ -> decode)))
                            (field "away" (lazy (\_ -> decode)))
                            (field "round" R.decode)
                            (field "hasQualified" decodeHasQualified)

                    _ ->
                        fail (node ++ " is not a recognized node for brackets")
            )


decodeWinner : Maybe String -> Decoder Winner
decodeWinner w =
    let
        stringToWinner wnnr =
            case wnnr of
                "HomeTeam" ->
                    HomeTeam

                "AwayTeam" ->
                    AwayTeam

                _ ->
                    None
    in
    case w of
        Nothing ->
            Json.Decode.succeed None

        Just wnr ->
            Json.Decode.succeed (stringToWinner wnr)


toStringHasQualified : HasQualified -> String
toStringHasQualified hasQ =
    case hasQ of
        TBD ->
            "TBD"

        In ->
            "In"

        Out ->
            "Out"


toHasQualified : String -> HasQualified
toHasQualified hasQStr =
    case hasQStr of
        "TBD" ->
            TBD

        "In" ->
            In

        "Out" ->
            Out

        _ ->
            TBD


encodeHasQualified : HasQualified -> Json.Encode.Value
encodeHasQualified hasQ =
    Json.Encode.string (toStringHasQualified hasQ)


decodeHasQualified : Decoder HasQualified
decodeHasQualified =
    Json.Decode.string
        |> Json.Decode.map toHasQualified
