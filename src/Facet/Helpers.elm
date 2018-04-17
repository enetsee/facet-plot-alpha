module Facet.Helpers exposing (minWith, maxWith, compareMaybe)


minWith : (a -> a -> Order) -> a -> a -> a
minWith compareWith a b =
    case compareWith a b of
        LT ->
            a

        _ ->
            b


maxWith : (a -> a -> Order) -> a -> a -> a
maxWith compareWith a b =
    case compareWith a b of
        GT ->
            a

        _ ->
            b


compareMaybe : (a -> Maybe b) -> (b -> b -> Order) -> a -> a -> Order
compareMaybe extract compareWith d1 d2 =
    case ( extract d1, extract d2 ) of
        ( Just v1, Just v2 ) ->
            compareWith v1 v2

        ( Just _, _ ) ->
            GT

        ( _, Just _ ) ->
            LT

        _ ->
            EQ
