module Lib.Cyclic exposing (Cyclic, current, cycle, fromList, moveTo, next, reset)

import List.Extra


type Cyclic a
    = Empty
    | Cyclic (List a) a (List a)


fromList : List a -> Cyclic a
fromList l =
    case l of
        [] ->
            Empty

        x :: xs ->
            Cyclic [] x xs


cycle : Cyclic a -> Cyclic a
cycle c =
    case c of
        Empty ->
            Empty

        Cyclic ls x rs ->
            case rs of
                [] ->
                    case ls of
                        [] ->
                            c

                        el :: lss ->
                            Cyclic [] el (List.reverse ls)

                er :: rss ->
                    Cyclic (x :: ls) er rss


current : Cyclic a -> Maybe a
current c =
    case c of
        Empty ->
            Nothing

        Cyclic _ a _ ->
            Just a


next : Cyclic a -> ( Maybe a, Cyclic a )
next c =
    let
        newC =
            cycle c
    in
    ( current newC, newC )


moveTo : Cyclic a -> (a -> Bool) -> Maybe (Cyclic a)
moveTo c f =
    case c of
        Empty ->
            Just Empty

        Cyclic ls e rs ->
            let
                m =
                    (e :: rs)
                        |> List.append (List.reverse ls)
                        |> List.Extra.splitWhen f
            in
            case m of
                Nothing ->
                    Nothing

                Just ( left, right ) ->
                    case right of
                        [] ->
                            -- should not happen according to splitWhen specs
                            Nothing

                        x :: xs ->
                            Just <| Cyclic left x xs


reset c =
    case c of
        Empty ->
            Empty

        Cyclic ls e rs ->
            (e :: rs)
                |> List.append (List.reverse ls)
                |> fromList
