module Activities exposing (..)

import Element exposing (alignLeft, alignRight, column, height, padding, paddingXY, px, row, spacing, width)
import Element.Events as Events
import Element.Input as Input
import Http
import Json.Decode exposing (Decoder, andThen, field, maybe)
import Json.Encode
import Markdown
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Web exposing (defaultConfig)
import Time
import Types exposing (ActivitiesModel, Activity(..), ActivityMeta, Comment, Msg(..), Post)
import UI.Button
import UI.Style
import UI.Text


fetchActivities : ActivitiesModel Msg -> Cmd Msg
fetchActivities model =
    Web.get "/activities" FetchedActivities decode


saveComment : ActivitiesModel Msg -> Cmd Msg
saveComment model =
    let
        comment =
            encodeComment model.comment
    in
    Web.post "/activities/comments" SavedComment decode comment


savePost : ActivitiesModel Msg -> String -> Cmd Msg
savePost model token =
    let
        post =
            encodePost model.post

        bearer =
            "Bearer " ++ token

        header =
            Http.header "Authorization" bearer

        config =
            { defaultConfig | headers = [ header ] }
    in
    Web.postWithConfig config "/activities/blogs" SavedPost decode post


viewActivities : Time.Zone -> WebData (List Activity) -> Element.Element Msg
viewActivities tz wActivities =
    case wActivities of
        NotAsked ->
            Element.text "Aan het ophalen."

        Loading ->
            Element.text "Aan het ophalen..."

        Failure err ->
            UI.Text.error "Oeps. Daar ging iets niet goed."

        Success activities ->
            List.map (viewActivity tz) activities
                |> Element.column [ spacing 20 ]


viewActivity : Time.Zone -> Activity -> Element.Element Msg
viewActivity tz activity =
    case activity of
        ANewBet activityMeta name uuid ->
            Element.el [ padding 20 ] (Element.text (name ++ "doet mee"))

        AComment activityMeta author comment ->
            commentBox author comment tz activityMeta.date

        APost activityMeta author blogTitle blog ->
            blogBox author blogTitle blog tz activityMeta.date

        ANewRanking activityMeta ->
            Element.el [ padding 20 ] (Element.text "De stand is bijgewerkt")


blogBox : String -> String -> String -> Time.Zone -> Time.Posix -> Element.Element Msg
blogBox author title blog tz dt =
    column
        [ padding 20 ]
        [ Element.paragraph [] [ Element.text title ]
        , blogView blog
        , Element.el [ alignRight ] (Element.text (author ++ ", " ++ UI.Text.dateText tz dt))
        ]


commentBox : String -> String -> Time.Zone -> Time.Posix -> Element.Element Msg
commentBox author comment tz dt =
    column
        [ padding 20 ]
        [ row
            [ alignLeft ]
            [ Element.el [] (Element.text (author ++ " zegt:")) ]
        , row
            [ alignLeft ]
            [ commentView comment ]
        , row
            [ alignRight ]
            [ timeView tz dt ]
        ]


blogView : String -> Element.Element Msg
blogView c =
    let
        comment =
            Markdown.toHtml [] c
                |> Element.html
    in
    Element.el [] comment


commentView : String -> Element.Element Msg
commentView c =
    let
        comment =
            Markdown.toHtml [] c
                |> Element.html
    in
    Element.el [] comment


timeView : Time.Zone -> Time.Posix -> Element.Element Msg
timeView tz dt =
    Element.el [] (Element.text (UI.Text.dateText tz dt))


viewCommentInput : ActivitiesModel Msg -> Element.Element Msg
viewCommentInput model =
    let
        commentInput v =
            let
                area =
                    { onChange = SetCommentMsg
                    , text = v
                    , placeholder = Nothing
                    , label = Input.labelAbove [] (Element.text "zeg wat")
                    , spellcheck = True
                    }
            in
            Input.multiline [ height (px 120) ] area

        commentInputTrap =
            Element.paragraph
                [ paddingXY 0 0, spacing 0 ]
                [ Element.text "Schrijf vooral iets op "
                , UI.Button.pill UI.Style.Active ShowCommentInput "het prikbord"
                ]

        commentInputTrap2 v =
            let
                area =
                    { onChange = SetCommentMsg
                    , text = ""
                    , label = Input.labelAbove [] (Element.text "Tekst")
                    , placeholder = Nothing
                    , spellcheck = True
                    }
            in
            Input.multiline [ Events.onFocus ShowCommentInput, height (px 36) ] area

        authorInput v =
            let
                area =
                    { onChange = SetCommentAuthor
                    , text = ""
                    , label = Input.labelAbove [] (Element.text "Naam")
                    , placeholder = Nothing
                    }
            in
            Input.text [ height (px 36) ] area

        saveButton =
            if (model.comment.msg == "") || (model.comment.author == "") then
                UI.Button.pill UI.Style.Inactive NoOp "je moet beide velden invullen"

            else
                UI.Button.pill UI.Style.Active SaveComment "prik!"

        input =
            if model.showComment then
                Element.column
                    [ padding 10, spacing 20 ]
                    [ commentInput model.comment.msg
                    , authorInput model.comment.author
                    , saveButton
                    ]

            else
                commentInputTrap

        -- Element.column UI.Style.CommentInputBox
        --     [ padding 10, spacing 20 ]
        --     [ commentInputTrap
        --     ]
    in
    Element.el [ paddingXY 0 0 ] input


viewPostInput : ActivitiesModel Msg -> Element.Element Msg
viewPostInput model =
    let
        titleInput v =
            let
                area =
                    { onChange = SetPostTitle
                    , text = v
                    , placeholder = Nothing
                    , label = Input.labelAbove [] (Element.text "Titel")
                    }
            in
            Input.text [ height (px 36) ] area

        postInput v =
            let
                area =
                    { onChange = SetPostMsg
                    , text = v
                    , placeholder = Nothing
                    , label = Input.labelAbove [] (Element.text "Tekst")
                    , spellcheck = True
                    }
            in
            Input.multiline [ height (px 200) ] area

        postInputTrap =
            let
                area =
                    { onChange = \_ -> NoOp
                    , label = Input.labelAbove [] (Element.text "Nieuwe Blog post.")
                    , text = ""
                    , placeholder = Nothing
                    }
            in
            Input.text [ Events.onFocus ShowPostInput, height (px 36) ] area

        passphraseInput v =
            let
                area =
                    { onChange = SetPostPassphrase
                    , label = Input.labelAbove [] (Element.text "Wachtwoord")
                    , text = v
                    , placeholder = Nothing
                    }
            in
            Input.text [ height (px 36) ] area

        authorInput v =
            let
                area =
                    { onChange = SetPostAuthor
                    , label = Input.labelAbove [] (Element.text "Naam")
                    , text = v
                    , placeholder = Nothing
                    }
            in
            Input.text [ height (px 36) ] area

        saveButton =
            if (model.post.msg == "") || (model.post.author == "") || (model.post.passphrase == "") then
                UI.Button.pill UI.Style.Inactive NoOp "je moet alle velden invullen"

            else
                UI.Button.pill UI.Style.Active SavePost "post!"

        input =
            if model.showPost then
                Element.column
                    [ padding 20, spacing 20 ]
                    [ titleInput model.post.title
                    , postInput model.post.msg
                    , passphraseInput model.post.passphrase
                    , authorInput model.post.author
                    , saveButton
                    ]

            else
                Element.column
                    [ padding 20, spacing 20 ]
                    [ postInputTrap
                    ]
    in
    Element.el [ paddingXY 20 20 ] input



-- Json


encodeComment : Comment -> Json.Encode.Value
encodeComment comment =
    let
        encodedComment =
            Json.Encode.object
                [ ( "author", Json.Encode.string comment.author )
                , ( "msg", encodeMessage comment.msg )
                ]

        multlineMsg =
            String.split "\n" comment.msg
    in
    Json.Encode.object
        [ ( "comment", encodedComment ) ]


encodePost : Post -> Json.Encode.Value
encodePost post =
    let
        encodedPost =
            Json.Encode.object
                [ ( "author", Json.Encode.string post.author )
                , ( "title", Json.Encode.string post.title )
                , ( "msg", encodeMessage post.msg )
                , ( "passphrase", Json.Encode.string post.passphrase )
                ]
    in
    Json.Encode.object
        [ ( "blog", encodedPost ) ]


encodeMessage : String -> Json.Encode.Value
encodeMessage msg =
    let
        multlineMsg =
            String.split "\n" msg
    in
    Json.Encode.list Json.Encode.string multlineMsg


encodeActivity : Activity -> Json.Encode.Value
encodeActivity activity =
    case activity of
        AComment am author msg ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "comment" )
                , ( "author", Json.Encode.string author )
                , ( "msg", encodeMessage msg )
                , ( "meta", encodeActivityMeta am )
                ]

        APost am author title msg ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "blog" )
                , ( "author", Json.Encode.string author )
                , ( "title", Json.Encode.string title )
                , ( "msg", encodeMessage msg )
                , ( "meta", encodeActivityMeta am )
                ]

        ANewRanking am ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "new-ranking" )
                , ( "meta", encodeActivityMeta am )
                ]

        ANewBet am name betUuid ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "comment" )
                , ( "name", Json.Encode.string name )
                , ( "bet-uuid", Json.Encode.string betUuid )
                , ( "meta", encodeActivityMeta am )
                ]


encodeActivityMeta : ActivityMeta -> Json.Encode.Value
encodeActivityMeta am =
    Json.Encode.object
        [ ( "date", Json.Encode.int (Time.posixToMillis am.date) )
        , ( "active", Json.Encode.bool am.active )
        , ( "uuid", Json.Encode.string am.uuid )
        ]


type alias IncomingActivities =
    { activities : List Activity }


decode : Decoder (List Activity)
decode =
    field "activities" (Json.Decode.list decodeActivity)


decodeActivity : Decoder Activity
decodeActivity =
    field "type" Json.Decode.string |> andThen decodeActivityDetails


decodeActivityDetails : String -> Decoder Activity
decodeActivityDetails tp =
    case tp of
        "comment" ->
            Json.Decode.map3 AComment
                (field "meta" decodeMeta)
                (field "author" Json.Decode.string)
                (field "msg" decodeMessage)

        "blog" ->
            Json.Decode.map4 APost
                (field "meta" decodeMeta)
                (field "author" Json.Decode.string)
                (field "title" Json.Decode.string)
                (field "msg" decodeMessage)

        "new-bet" ->
            Json.Decode.map3 ANewBet
                (field "meta" decodeMeta)
                (field "name" Json.Decode.string)
                (field "bet-uuid" Json.Decode.string)

        "new-ranking" ->
            Json.Decode.map ANewRanking
                (field "meta" decodeMeta)

        _ ->
            Json.Decode.fail "WHOOPS"


decodeMeta : Decoder ActivityMeta
decodeMeta =
    Json.Decode.map3 ActivityMeta
        (field "date" decodeDate)
        (field "active" Json.Decode.bool)
        (field "uuid" Json.Decode.string)


decodeDate : Decoder Time.Posix
decodeDate =
    Json.Decode.int
        |> Json.Decode.map Time.millisToPosix


decodeMessage : Decoder String
decodeMessage =
    Json.Decode.list Json.Decode.string
        |> Json.Decode.map (String.join "\n")