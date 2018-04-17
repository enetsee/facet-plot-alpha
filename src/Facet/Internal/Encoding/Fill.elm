module Facet.Internal.Encoding.Fill
    exposing
        ( Fill
        , containsAggregate
        , containsVector
        , extractOrElse
        , extractVectorOrElse
        , summarizeOrElse
        , summarize
        , extract
        , extractVector
        , compareAt
        , equalAt
        , empty
        )

{-|
@docs Fill, containsAggregate, extractOrElse, summarizeOrElse
-}

import Color exposing (Color)
import Facet.Internal.Channel as Channel exposing (ColorChannel, FloatChannel)
import Facet.Maybe.Extra as Maybe
import Facet.Scenegraph.Fill as Mark


{-| -}
type alias Fill data =
    { fill : Maybe (ColorChannel data)
    , fillOpacity : Maybe (FloatChannel data)
    }


empty : Fill data
empty =
    { fill = Nothing
    , fillOpacity = Nothing
    }


compareAt :
    Fill data
    -> data
    -> data
    -> Order
compareAt { fill, fillOpacity } d1 d2 =
    case Channel.compareMaybeAt fill d1 d2 of
        EQ ->
            Channel.compareMaybeAt fillOpacity d1 d2

        otherwise ->
            otherwise


equalAt :
    Fill data
    -> data
    -> data
    -> Bool
equalAt { fill, fillOpacity } d1 d2 =
    Channel.equalMaybeAt fill d1 d2 && Channel.equalMaybeAt fillOpacity d1 d2


{-| -}
containsAggregate : Fill data -> Bool
containsAggregate { fill, fillOpacity } =
    Maybe.maybe False Channel.isAggregate fill
        || Maybe.maybe False Channel.isAggregate fillOpacity


containsVector : Fill data -> Bool
containsVector { fill, fillOpacity } =
    Maybe.maybe False Channel.isVector fill
        || Maybe.maybe False Channel.isVector fillOpacity


{-| -}
extractOrElse : Mark.Fill -> Fill data -> data -> Mark.Fill
extractOrElse mark fill datum =
    let
        f a b =
            Maybe.orElse a <|
                Maybe.andThen (\ch -> Channel.extract ch datum)
                    b
    in
        { fill = f mark.fill fill.fill
        , fillOpacity = f mark.fillOpacity fill.fillOpacity
        }


extract : Fill data -> data -> Mark.Fill
extract fill datum =
    { fill = Maybe.andThen (\ch -> Channel.extract ch datum) fill.fill
    , fillOpacity = Maybe.andThen (\ch -> Channel.extract ch datum) fill.fillOpacity
    }


{-| -}
extractVectorOrElse : Mark.Fill -> Fill data -> List data -> Mark.Fill
extractVectorOrElse mark fill data =
    let
        f a b =
            b
                |> Maybe.andThen (\ch -> Channel.extractVector ch data |> List.head |> Maybe.join)
                |> Maybe.orElse a
    in
        { fill = f mark.fill fill.fill
        , fillOpacity = f mark.fillOpacity fill.fillOpacity
        }


extractVector : Fill data -> List data -> List Mark.Fill
extractVector fill data =
    let
        fs =
            fill.fill
                |> Maybe.map (\ch -> Channel.extractVector ch data)

        os =
            fill.fillOpacity
                |> Maybe.map (\ch -> Channel.extractVector ch data)
    in
        case ( fs, os ) of
            ( Just fills, Just opacities ) ->
                extractVectorHelper [] fills opacities

            ( Just fills, _ ) ->
                fills |> List.map (\fill -> { fill = fill, fillOpacity = Nothing })

            ( _, Just opacities ) ->
                opacities |> List.map (\fillOpacity -> { fill = Nothing, fillOpacity = fillOpacity })

            _ ->
                []


extractVectorHelper :
    List Mark.Fill
    -> List (Maybe Color)
    -> List (Maybe Float)
    -> List Mark.Fill
extractVectorHelper accu fills opacities =
    case ( fills, opacities ) of
        ( [], [] ) ->
            List.reverse accu

        ( nextFill :: restFills, nextOpacity :: restOpacities ) ->
            extractVectorHelper ((Mark.Fill nextFill nextOpacity) :: accu)
                restFills
                restOpacities

        ( nextFill :: restFills, [] ) ->
            extractVectorHelper ((Mark.Fill nextFill Nothing) :: accu)
                restFills
                []

        ( [], nextOpacity :: restOpacities ) ->
            extractVectorHelper ((Mark.Fill Nothing nextOpacity) :: accu)
                []
                restOpacities


summarize : Fill data -> List data -> Mark.Fill
summarize fill datum =
    { fill = Maybe.andThen (\ch -> Channel.summarize ch datum) fill.fill
    , fillOpacity = Maybe.andThen (\ch -> Channel.summarize ch datum) fill.fillOpacity
    }


{-| -}
summarizeOrElse : Mark.Fill -> Fill data -> List data -> Mark.Fill
summarizeOrElse mark fill data =
    let
        f a b =
            Maybe.orElse a <|
                Maybe.andThen (\ch -> Channel.summarize ch data)
                    b
    in
        { fill = f mark.fill fill.fill
        , fillOpacity = f mark.fillOpacity fill.fillOpacity
        }
