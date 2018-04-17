module Facet.Maybe.Extra exposing (orElse, maybe, maybeLazy, isNothing, isJust, join, toList)


toList : Maybe a -> List a
toList maybeVal =
    case maybeVal of
        Just x ->
            [ x ]

        _ ->
            []


join : Maybe (Maybe a) -> Maybe a
join maybeMaybeVal =
    case maybeMaybeVal of
        Just (Just val) ->
            Just val

        _ ->
            Nothing


isJust : Maybe a -> Bool
isJust maybeVal =
    case maybeVal of
        Just _ ->
            True

        _ ->
            False


isNothing : Maybe a -> Bool
isNothing maybeVal =
    case maybeVal of
        Nothing ->
            True

        _ ->
            False


orElse : Maybe a -> Maybe a -> Maybe a
orElse ifNothing maybeVal =
    case maybeVal of
        Just _ ->
            maybeVal

        _ ->
            ifNothing


maybe : a -> (b -> a) -> Maybe b -> a
maybe ifNothing withJust maybeVal =
    case maybeVal of
        Just x ->
            withJust x

        _ ->
            ifNothing


maybeLazy : (() -> a) -> (b -> a) -> Maybe b -> a
maybeLazy ifNothing withJust maybeVal =
    case maybeVal of
        Just x ->
            withJust x

        _ ->
            ifNothing ()
