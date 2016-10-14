module ListUtil exposing (..)

takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
    case list of
        [] -> []
        x::xs ->
            if predicate x then
                x :: takeWhile predicate xs
            else
                []


dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] -> []
        x::xs ->
            if predicate x then
                dropWhile predicate xs
            else
                list
