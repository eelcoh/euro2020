module Types exposing
    ( Access(..)
    , ActivitiesModel
    , Activity(..)
    , ActivityMeta
    , App(..)
    , BetSummaryLine
    , Card(..)
    , Comment
    , Credentials(..)
    , DataStatus(..)
    , Flags
    , FormInfoMsg(..)
    , Info(..)
    , InputState(..)
    , KnockoutsResults
    , MatchResult
    , MatchResults
    , Model
    , Msg(..)
    , Page
    , Post
    , Qualified(..)
    , RankingDetails
    , RankingGroup
    , RankingSummary
    , RankingSummaryLine
    , RoundScore
    , TeamRounds
    , Toggle(..)
    , Token(..)
    , TopscorerResults
    , UUID
    , init
    , initComment
    , initPost
    )

import Bets.Init
import Bets.Types exposing (Bet, Group(..), Round(..), Topscorer)
import Browser
import Browser.Navigation as Navigation
import Form.Bracket.Types as Bracket
import Form.GroupMatches.Types as GroupMatches
import Form.Participant.Types as Participant
import Form.Topscorer.Types as Topscorer
import Html exposing (Html, div)
import RemoteData exposing (RemoteData(..), WebData)
import Time
import UI.Button exposing (Size(..))
import UI.Screen as Screen
import Url


type App
    = Home
    | Blog
    | Form
    | Login
    | Logoff
    | Bets
    | Ranking
    | RankingDetailsView
    | Results
    | TSResults
    | KOResults
    | EditMatchResult
    | BetsDetailsView


type alias Flags =
    { formId : Maybe String
    , width : Int
    , height : Int
    , token : Maybe String
    }


type Card
    = IntroCard Info
      -- | QuestionCard Questions.Model
    | GroupMatchesCard GroupMatches.State
    | BracketCard Bracket.State
    | BracketKnockoutsCard Bracket.State
    | TopscorerCard
    | ParticipantCard
    | SubmitCard


type Info
    = Intro
    | FirstRoundIntro
    | GroupIntro Group
    | GroupStandingsIntro Group
    | BestThirdIntro
    | BracketIntro
    | BracketView
    | BracketRoundIntro Round
    | TopscorerIntro
    | AboutYouIntro
    | ThankYou


type alias Page =
    Int


type InputState
    = Clean
    | Dirty


init : Maybe String -> Screen.Size -> Navigation.Key -> Maybe String -> Model Msg
init formId sz navKey tokenStr =
    let
        token =
            case tokenStr of
                Nothing ->
                    NotAsked

                Just t ->
                    Success (Token t)
    in
    { cards = initCards sz
    , bet = Bets.Init.bet
    , savedBet = NotAsked
    , idx = 0
    , formId = formId
    , betState = Clean
    , navKey = navKey
    , screen = sz
    , activities = activitiesInit
    , app = Home
    , token = token
    , credentials = Empty
    , bets = NotAsked
    , ranking = NotAsked
    , rankingDetails = NotAsked
    , matchResults = NotAsked
    , matchResult = NotAsked
    , knockoutsResults = Fresh NotAsked
    , topscorerResults = Fresh NotAsked
    , timeZone = Time.utc
    }


type alias Model msg =
    { cards : List Card
    , bet : Bet
    , savedBet : WebData Bet
    , idx : Page
    , formId : Maybe String
    , betState : InputState
    , navKey : Navigation.Key
    , screen : Screen.Size
    , activities : ActivitiesModel msg
    , app : App
    , token : WebData Token
    , credentials : Credentials
    , bets : WebData Bets
    , ranking : WebData RankingSummary
    , rankingDetails : WebData RankingDetails
    , matchResults : WebData MatchResults
    , matchResult : WebData MatchResult
    , knockoutsResults : DataStatus (WebData KnockoutsResults)
    , topscorerResults : DataStatus (WebData TopscorerResults)
    , timeZone : Time.Zone
    }


type FormInfoMsg
    = NoOps


type Msg
    = NavigateTo Page
    | SetApp App
      -- | Answered Page Form.Question.Msg
    | FoundTimeZone Time.Zone
    | InfoMsg FormInfoMsg
      -- | QuestionSetMsg Page Form.QuestionSet.Msg
    | GroupMatchMsg Group GroupMatches.Msg
    | BracketMsg Bracket.Msg
    | TopscorerMsg Topscorer.Msg
    | ParticipantMsg Participant.Msg
    | SubmitMsg
    | SubmittedBet (WebData Bet)
    | BetSelected UUID
    | FetchedBet (WebData Bet)
    | NoOp
    | Restart
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | ScreenResize Int Int
      --
    | RefreshActivities
    | FetchedActivities (WebData (List Activity))
    | SetCommentMsg String
    | SetCommentAuthor String
    | SaveComment
    | SavedComment (WebData (List Activity))
    | HideCommentInput
    | ShowCommentInput
    | SetPostTitle String
    | SetPostPassphrase String
    | SetPostMsg String
    | SetPostAuthor String
    | SavePost
    | SavedPost (WebData (List Activity))
    | HidePostInput
    | ShowPostInput
      -- Authenticate
    | FetchedToken (WebData Token)
    | SetUsername String
    | SetPassword String
    | Authenticate
    | Logout
      -- Bets
    | FetchBets
    | FetchedBets (WebData Bets)
    | ToggleBetActiveFlag UUID Toggle
    | ToggledBetActiveFlag (WebData Bet)
      -- Ranking
    | RecreateRanking
    | FetchedRanking (WebData RankingSummary)
    | RefreshRanking
    | FetchedRankingDetails (WebData RankingDetails)
    | ViewRankingDetails String
    | RetrieveRankingDetails String
      -- Match Results
    | InitialiseMatchResults
    | FetchedMatchResults (WebData MatchResults)
    | RefreshResults
    | EditMatch MatchResult
    | UpdateMatchResult MatchResult
    | CancelMatchResult MatchResult
    | StoredMatchResult (WebData MatchResult)
      -- Knockouts
    | FetchedKnockoutsResults (WebData KnockoutsResults)
    | StoredKnockoutsResults (WebData KnockoutsResults)
    | Qualify Bets.Types.Round Bets.Types.HasQualified Bets.Types.Team
    | UpdateKnockoutsResults
    | InitialiseKnockoutsResults
    | RefreshKnockoutsResults
    | ChangeQualify Bets.Types.Round Bets.Types.HasQualified Bets.Types.Team
      -- Topscorer
    | RefreshTopscorerResults
    | ChangeTopscorerResults Bets.Types.HasQualified Topscorer
    | UpdateTopscorerResults
    | InitialiseTopscorerResults
    | FetchedTopscorerResults (WebData TopscorerResults)
    | StoredTopscorerResults (WebData TopscorerResults)


type Access
    = Unauthorised
    | Authorised


initCards : Screen.Size -> List Card
initCards sz =
    [ IntroCard Intro
    , GroupMatchesCard <| GroupMatches.init A "m01"
    , GroupMatchesCard <| GroupMatches.init B "m03"
    , GroupMatchesCard <| GroupMatches.init C "m05"
    , GroupMatchesCard <| GroupMatches.init D "m07"
    , GroupMatchesCard <| GroupMatches.init E "m09"
    , GroupMatchesCard <| GroupMatches.init F "m11"
    , BracketCard <| Bracket.init sz
    , BracketKnockoutsCard <| Bracket.initialKnockouts sz
    , TopscorerCard
    , ParticipantCard
    , SubmitCard
    ]



-- Activities


type alias ActivitiesModel msg =
    { activities : WebData (List Activity)
    , comment : Comment
    , post : Post
    , contents : Html msg
    , showComment : Bool
    , showPost : Bool
    }


activitiesInit : ActivitiesModel Msg
activitiesInit =
    { activities = NotAsked
    , comment = initComment
    , post = initPost
    , contents = div [] []
    , showComment = False
    , showPost = False
    }


initComment : Comment
initComment =
    { author = "", msg = "" }


initPost : Post
initPost =
    { author = "", title = "", msg = "", passphrase = "" }


type alias Name =
    String


type alias Author =
    String


type alias Title =
    String


type alias Message =
    String


type alias UUID =
    String


type alias ActivityMeta =
    { date : Time.Posix
    , active : Bool
    , uuid : String
    }


type Activity
    = ANewBet ActivityMeta Name UUID
    | AComment ActivityMeta Author Message
    | APost ActivityMeta Author Title Message
    | ANewRanking ActivityMeta


type alias Comment =
    { author : String
    , msg : String
    }


type alias Post =
    { author : String
    , title : String
    , msg : String
    , passphrase : String
    }



--


type Credentials
    = Empty
    | WithUsername String
    | WithPassword String
    | Submittable String String


type Token
    = Token String



-- Ranking


type alias RankingSummary =
    { summary : List RankingGroup
    , time : Time.Posix
    }


type alias RankingGroup =
    { pos : Int
    , bets : List RankingSummaryLine
    , total : Int
    }



-- Bets


type Toggle
    = ToggleInactive
    | ToggleActive


type alias Bets =
    List BetSummaryLine


type alias BetSummaryLine =
    { name : String
    , active : Bool
    , uuid : String
    }


type alias RankingSummaryLine =
    { name : String
    , rounds : List RoundScore
    , topscorer : Int
    , total : Int
    , uuid : String
    }


type alias RankingDetails =
    { name : String
    , rounds : List RoundScore
    , topscorer : Int
    , total : Int
    , uuid : String
    , bet : Bets.Types.Bet
    }


type alias RoundScore =
    { round : String
    , points : Int
    }



-- Results


type alias MatchResults =
    { results : List MatchResult }


type alias MatchResult =
    { matchResultId : String
    , match : String
    , homeTeam : Bets.Types.Team
    , awayTeam : Bets.Types.Team
    , score : Maybe Bets.Types.Score
    }



-- Knockouts


type alias KnockoutsResults =
    { teams : List ( String, TeamRounds )
    }


type alias TeamRounds =
    { team : Bets.Types.Team
    , roundsQualified : List ( Bets.Types.Round, Bets.Types.HasQualified )
    }


type DataStatus a
    = Fresh a
    | Filthy a
    | Stale a


type Qualified
    = Did
    | DidNot
    | NotYet



-- Topscorer


type alias TopscorerResults =
    { topscorers : List ( Bets.Types.HasQualified, Topscorer )
    }
