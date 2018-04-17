module Facet.Internal.Encoding.Polygon exposing (Polygon, polygon, scenegraph, legends)

import Facet.Internal.Encoding.Fill as Fill exposing (Fill)
import Facet.Internal.Encoding.Stroke as Stroke exposing (Stroke)
import Facet.Internal.Channel as Channel exposing (TextChannel, FloatChannel, PositionalChannel)
import Facet.Internal.Field as Field
import Facet.Internal.Legend exposing (LegendSpec)
import Facet.List.Extra as List
import Facet.Maybe.Extra as Maybe
import Facet.Scale as Scale exposing (Scale)
import Facet.Scenegraph as Scenegraph exposing (Scenegraph)
import Facet.Scenegraph.Cursor as Cursor exposing (Cursor)
import Facet.Scenegraph.Fill as Scenegraph
import Facet.Scenegraph.Stroke as Scenegraph
import Facet.Scenegraph.Interpolate exposing (Interpolate(..))
import Facet.Scenegraph.Mark as Mark exposing (Behaviour(..), Orientation(..))
import Facet.Theme as Theme


type alias Polygon data xdomain ydomain =
    { x : PositionalChannel data xdomain
    , y : PositionalChannel data ydomain
    , interpolate : Interpolate
    , behavior : Mark.Behaviour
    , fill : Maybe (Fill data)
    , stroke : Maybe (Stroke data)
    , cursor : Maybe Cursor
    , href : Maybe String
    , tooltip : Maybe (TextChannel data)
    }



-- Helpers ---------------------------------------------------------------------


polygon :
    PositionalChannel data xdomain
    -> PositionalChannel data number
    -> Interpolate
    -> Behaviour
    -> Polygon data xdomain number
polygon x y interpolate behaviour =
    Polygon x
        y
        interpolate
        behaviour
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing



-- LEGENDS ---------------------------------------------------------------------


legends : Int -> Polygon data xdomain ydomain -> LegendSpec
legends ticks encoding =
    { fillColor = encoding.fill |> Maybe.andThen .fill |> Maybe.andThen (Channel.legend ticks)
    , fillOpacity = encoding.fill |> Maybe.andThen .fillOpacity |> Maybe.andThen (Channel.legend ticks)
    , strokeColor = encoding.stroke |> Maybe.andThen .stroke |> Maybe.andThen (Channel.legend ticks)
    , strokeOpacity = encoding.stroke |> Maybe.andThen .strokeOpacity |> Maybe.andThen (Channel.legend ticks)
    , strokeWidth = encoding.stroke |> Maybe.andThen .strokeWidth |> Maybe.andThen (Channel.legend ticks)
    , strokeDash = encoding.stroke |> Maybe.andThen .strokeDash |> Maybe.andThen (Channel.legend ticks)
    , angle = Nothing
    , shape = Nothing
    , cornerRadius = Nothing
    , size = Nothing
    , width = Nothing
    }



-- SCENEGRAPH ------------------------------------------------------------------


{-| An intermediate representation of an encoding which allows for all
    fields to possibly be `Nothing`
-}
type alias PolygonInternal =
    { x : Maybe (List (Maybe Float))
    , y : Maybe (List (Maybe Float))
    , fill : Maybe Scenegraph.Fill
    , stroke : Maybe Scenegraph.Stroke
    , cursor : Maybe Cursor
    , href : Maybe String
    , tooltip : Maybe String
    }


{-| Combine `PolygonInternal`s with a preference for `mark2` (in practice,
    for each field in the two `RectInternal`s, only one _can_ have a `Just`
    value so the order of arguments does not matter.)
-}
combineIntermediate : PolygonInternal -> PolygonInternal -> PolygonInternal
combineIntermediate mark1 mark2 =
    { x = mark2.x |> Maybe.orElse mark1.x
    , y = Maybe.orElse mark1.y mark2.y
    , fill = Maybe.orElse mark1.fill mark2.fill
    , stroke = Maybe.orElse mark1.stroke mark2.stroke
    , cursor = Maybe.orElse mark1.cursor mark2.cursor
    , href = Maybe.orElse mark1.href mark2.href
    , tooltip = Maybe.orElse mark1.tooltip mark2.tooltip
    }


{-| Combine an `PathInternal` with required fields from the encoding
    and a theme to populate any un-encoded fields.
-}
combineWithThemeDefaults : Theme.Polygon -> Interpolate -> Behaviour -> PolygonInternal -> Maybe Mark.Polygon
combineWithThemeDefaults theme interpolate behaviour markInternal =
    Maybe.map2
        (\x y ->
            { x = x
            , y = y
            , interpolate = interpolate
            , behaviour = behaviour
            , fill = Maybe.withDefault theme.fill markInternal.fill
            , stroke = Maybe.withDefault theme.stroke markInternal.stroke
            , cursor = Maybe.withDefault theme.cursor markInternal.cursor
            , href = markInternal.href
            , tooltip = markInternal.tooltip
            }
        )
        markInternal.x
        markInternal.y


{-| Test for an aggregate field in one of the channels
-}
containsAggregate : Polygon data xdomain ydomain -> Bool
containsAggregate encoding =
    Channel.isAggregate encoding.x
        || Channel.isAggregate encoding.y
        || Maybe.maybe False Fill.containsAggregate encoding.fill
        || Maybe.maybe False Stroke.containsAggregate encoding.stroke
        || Maybe.maybe False Channel.isAggregate encoding.tooltip


containsVector : Polygon data xdomain ydomain -> Bool
containsVector encoding =
    Channel.isVector encoding.x
        || Channel.isVector encoding.y
        || Maybe.maybe False Fill.containsVector encoding.fill
        || Maybe.maybe False Stroke.containsVector encoding.stroke
        || Maybe.maybe False Channel.isVector encoding.tooltip



{- Generate a scenegraph for the encoding -}


scenegraph :
    Theme.Area
    -> Scale xdomain Float
    -> Scale ydomain Float
    -> List data
    -> Polygon data xdomain ydomain
    -> Scenegraph
scenegraph theme xScale yScale data encoding =
    if containsAggregate encoding then
        data
            |> List.groupBy (compareAt encoding)
                (extract xScale yScale encoding)
            |> List.filterMap
                (\( mark, data ) ->
                    let
                        vector =
                            extractVector xScale yScale encoding data

                        agg =
                            summarize xScale yScale encoding data
                    in
                        mark
                            |> combineIntermediate vector
                            |> combineIntermediate agg
                            |> combineWithThemeDefaults theme encoding.interpolate encoding.behavior
                )
            |> Scenegraph.Polygon
    else if containsVector encoding then
        data
            |> List.groupBy (compareAt encoding)
                (extract xScale yScale encoding)
            |> List.filterMap
                (\( mark, data ) ->
                    let
                        vector =
                            extractVector xScale yScale encoding data
                    in
                        mark
                            |> combineIntermediate vector
                            |> combineWithThemeDefaults theme encoding.interpolate encoding.behavior
                )
            |> Scenegraph.Polygon
    else
        List.filterMap
            (extract xScale yScale encoding
                >> combineWithThemeDefaults theme encoding.interpolate encoding.behavior
            )
            data
            |> Scenegraph.Polygon



-- APPLY AGGREGATE, VECTOR AND SCALAR FIELDS -----------------------------------


{-| Apply each scalar channel of an encoding to a data point.
    Where a channel is not a scalar, the corresponding field in the returned
    `RectInternal` is `Nothing`.
-}
extract :
    Scale xdomain Float
    -> Scale ydomain Float
    -> Polygon data xdomain ydomain
    -> data
    -> PolygonInternal
extract xScale yScale encoding datum =
    let
        extractMaybe getter =
            getter encoding
                |> Maybe.andThen (\channel -> Channel.extract channel datum)

        positionX =
            Field.extract encoding.x.field datum
                |> Maybe.andThen (Scale.scale xScale >> Maybe.map (Just >> List.singleton))

        positionY =
            Field.extract encoding.y.field datum
                |> Maybe.andThen (Scale.scale yScale >> Maybe.map (Just >> List.singleton))

        fill =
            Maybe.map (\fill -> Fill.extract fill datum) encoding.fill

        stroke =
            Maybe.map (\stroke -> Stroke.extract stroke datum) encoding.stroke
    in
        { x = positionX
        , y = positionY
        , fill = fill
        , stroke = stroke
        , cursor = encoding.cursor
        , href = encoding.href
        , tooltip = extractMaybe .tooltip
        }


{-| Apply each vector channel of an encoding to a data point.
    Where a channel is not a vector, the corresponding field in the returned
    `RectInternal` is `Nothing`.
-}
extractVector :
    Scale xdomain Float
    -> Scale ydomain Float
    -> Polygon data xdomain ydomain
    -> List data
    -> PolygonInternal
extractVector xScale yScale encoding data =
    let
        xs =
            Field.extractVector encoding.x.field data
                |> List.map (Maybe.andThen (Scale.scale xScale))

        ys =
            Field.extractVector encoding.y.field data
                |> List.map (Maybe.andThen (Scale.scale yScale))

        xys =
            List.map2
                (\x y ->
                    case Maybe.map2 (,) x y of
                        Just ( x, y ) ->
                            ( Just x, Just y )

                        _ ->
                            ( Nothing, Nothing )
                )
                xs
                ys

        ( xout, yout ) =
            case xys of
                [] ->
                    ( Nothing, Nothing )

                _ ->
                    let
                        ( x, y ) =
                            List.unzip xys
                    in
                        ( Just x, Just y )

        fill =
            Maybe.andThen
                (\fill -> Fill.extractVector fill data |> List.head)
                encoding.fill

        stroke =
            Maybe.andThen
                (\stroke -> Stroke.extractVector stroke data |> List.head)
                encoding.stroke

        tooltip =
            encoding.tooltip
                |> Maybe.andThen
                    (\channel ->
                        Channel.extractVector channel data
                            |> List.head
                            >> Maybe.join
                    )
    in
        { x = xout
        , y = yout
        , fill = fill
        , stroke = stroke
        , cursor = encoding.cursor
        , href = encoding.href
        , tooltip = tooltip
        }


{-| Apply each aggregate channel of an encoding to a data point.
    Where a channel is not an aggregate, the corresponding field in the returned
    `RectInternal` is `Nothing`.
-}
summarize :
    Scale xdomain Float
    -> Scale ydomain Float
    -> Polygon data xdomain ydomain
    -> List data
    -> PolygonInternal
summarize xScale yScale encoding data =
    let
        summarizeMaybe getter =
            getter encoding
                |> Maybe.andThen (\channel -> Channel.summarize channel data)

        positionX =
            Field.summarize encoding.x.field data
                |> Maybe.andThen (Scale.scale xScale >> Maybe.map (Just >> List.singleton))

        positionY =
            Field.summarize encoding.y.field data
                |> Maybe.andThen (Scale.scale yScale >> Maybe.map (Just >> List.singleton))
    in
        { x = positionX
        , y = positionY
        , fill = Maybe.map (\fill -> Fill.summarize fill data) encoding.fill
        , stroke = Maybe.map (\stroke -> Stroke.summarize stroke data) encoding.stroke
        , cursor = encoding.cursor
        , href = encoding.href
        , tooltip = summarizeMaybe .tooltip
        }


{-| Structural comparison of encoding domain evaluated at two data poinst
-}
compareAt : Polygon data xdomain ydomain -> data -> data -> Order
compareAt encoding d1 d2 =
    let
        comp getter =
            Channel.compareMaybeAt (getter encoding) d1 d2
    in
        case Channel.compareAt encoding.x d1 d2 of
            EQ ->
                case Channel.compareAt encoding.y d1 d2 of
                    EQ ->
                        case Maybe.maybe EQ (\fill -> Fill.compareAt fill d1 d2) encoding.fill of
                            EQ ->
                                case Maybe.maybe EQ (\stroke -> Stroke.compareAt stroke d1 d2) encoding.stroke of
                                    EQ ->
                                        comp .tooltip

                                    otherwise ->
                                        otherwise

                            otherwise ->
                                otherwise

                    otherwise ->
                        otherwise

            otherwise ->
                otherwise


equalAt : Polygon data xdomain ydomain -> data -> data -> Bool
equalAt encoding d1 d2 =
    let
        eq getter =
            Channel.equalMaybeAt (getter encoding) d1 d2
    in
        Channel.equalAt encoding.x d1 d2
            && Channel.equalAt encoding.y d1 d2
            && Maybe.maybe True (\fill -> Fill.equalAt fill d1 d2) encoding.fill
            && Maybe.maybe True (\stroke -> Stroke.equalAt stroke d1 d2) encoding.stroke
            && eq .tooltip
