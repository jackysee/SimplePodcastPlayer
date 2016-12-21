module ListUtil exposing (..)


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                x :: takeWhile predicate xs
            else
                []


dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                dropWhile predicate xs
            else
                list


swapDown : a -> List a -> List a
swapDown n list =
    case list of
        x :: y :: xs ->
            if x == n then
                y :: x :: xs
            else
                [ x ] ++ swapDown n (y :: xs)

        _ ->
            list


swapUp : a -> List a -> List a
swapUp n list =
    case list of
        x :: y :: xs ->
            if y == n then
                y :: x :: xs
            else
                [ x ] ++ swapUp n (y :: xs)

        _ ->
            list


getNext : (a -> Bool) -> List a -> Maybe a
getNext predicate list =
    case list of
        x :: y :: xs ->
            if predicate x then
                Just y
            else
                getNext predicate (y :: xs)

        _ ->
            Nothing


getPrev : (a -> Bool) -> List a -> Maybe a
getPrev predicate list =
    list
        |> List.reverse
        |> getNext predicate


findFirst : (a -> Bool) -> List a -> Maybe a
findFirst predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just x
            else
                findFirst predicate xs


findIndex : Int -> List a -> Maybe a
findIndex index list =
    list
        |> List.indexedMap (,)
        |> findFirst (\( i, a ) -> i == index)
        |> Maybe.map Tuple.second
