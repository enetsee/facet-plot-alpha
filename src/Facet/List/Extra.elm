module Facet.List.Extra exposing (find, groupBy, unique, chop, splitAt, fromMaybe, group, zipCycle, transpose)


transpose : List (List a) -> List (List a)
transpose ll =
    case ll of
        [] ->
            []

        [] :: xss ->
            transpose xss

        (x :: xs) :: xss ->
            let
                heads =
                    List.filterMap List.head xss

                tails =
                    List.filterMap List.tail xss
            in
                (x :: heads) :: transpose (xs :: tails)


fromMaybe : Maybe a -> List a
fromMaybe maybeVal =
    case maybeVal of
        Just x ->
            [ x ]

        _ ->
            []


chop : Maybe Int -> List a -> List (List a)
chop maybeLimit list =
    case maybeLimit of
        Nothing ->
            [ list ]

        Just limit ->
            case splitAt limit list of
                ( _, [] ) ->
                    [ list ]

                ( xs, ys ) ->
                    xs :: (chop maybeLimit ys)


splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    if n <= 0 then
        ( xs, [] )
    else
        splitAtHelper n [] xs


splitAtHelper : Int -> List a -> List a -> ( List a, List a )
splitAtHelper n accu xs =
    if n <= 0 then
        ( List.reverse accu, xs )
    else
        case xs of
            next :: rest ->
                splitAtHelper (n - 1) (next :: accu) rest

            _ ->
                ( List.reverse accu, [] )


zipCycle : List a -> List b -> List ( a, b )
zipCycle xs ys =
    case ( xs, ys ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        _ ->
            zipCycleHelper xs ys [] xs ys


zipCycleHelper :
    List a
    -> List b
    -> List ( a, b )
    -> List a
    -> List b
    -> List ( a, b )
zipCycleHelper xs ys accu cx cy =
    case ( cx, cy ) of
        ( nextX :: restX, nextY :: restY ) ->
            zipCycleHelper xs ys (( nextX, nextY ) :: accu) restX restY

        ( _ :: _, _ ) ->
            zipCycleHelper xs ys accu cx ys

        ( _, _ :: _ ) ->
            zipCycleHelper xs ys accu xs cy

        _ ->
            List.reverse accu


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        next :: rest ->
            if predicate next then
                Just next
            else
                find predicate rest


groupWhile : (a -> a -> Bool) -> List a -> List (List a)
groupWhile eq xs_ =
    case xs_ of
        [] ->
            []

        x :: xs ->
            let
                ( ys, zs ) =
                    span (eq x) xs
            in
                (x :: ys) :: groupWhile eq zs


unique : (a -> a -> Order) -> List a -> List a
unique compareWith list =
    list |> List.sortWith compareWith |> groupWhile (==) |> List.filterMap List.head


group : (a -> a -> Order) -> List a -> List (List a)
group compareWith list =
    list
        |> List.sortWith compareWith
        |> groupWhile (\a b -> compareWith a b == EQ)


groupBy : (a -> a -> Order) -> (a -> b) -> List a -> List ( b, List a )
groupBy compareWith projection list =
    list
        |> List.sortWith compareWith
        |> groupWhile (\x y -> projection x == projection y)
        |> List.filterMap (\xs -> List.head xs |> Maybe.map (\x -> ( projection x, xs )))


span : (a -> Bool) -> List a -> ( List a, List a )
span p xs =
    ( takeWhile p xs, dropWhile p xs )


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate =
    let
        takeWhileMemo memo list =
            case list of
                [] ->
                    List.reverse memo

                x :: xs ->
                    if (predicate x) then
                        takeWhileMemo (x :: memo) xs
                    else
                        List.reverse memo
    in
        takeWhileMemo []


dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if (predicate x) then
                dropWhile predicate xs
            else
                list
