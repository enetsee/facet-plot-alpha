module Facet.Internal.Encoding.Rect exposing (Rect, bar, rect, scenegraph, legends)

import Facet.Internal.Channel as Channel exposing (FloatChannel, PositionalChannel, TextChannel)
import Facet.Internal.Encoding.Fill as Fill exposing (Fill)
import Facet.Internal.Encoding.Position as Position exposing (Position(..))
import Facet.Internal.Encoding.Stroke as Stroke exposing (Stroke)
import Facet.Internal.Legend exposing (LegendSpec)
import Facet.Maybe.Extra as Maybe
import Facet.List.Extra as List
import Facet.Scale as Scale exposing (Scale)
import Facet.Scenegraph as Scenegraph exposing (Scenegraph)
import Facet.Scenegraph.Mark as Mark
import Facet.Scenegraph.Cursor as Cursor exposing (Cursor)
import Facet.Scenegraph.Fill as Scenegraph
import Facet.Scenegraph.Stroke as Scenegraph
import Facet.Scenegraph.Position as ScenegraphPosition
import Facet.Theme as Theme


{-| Rectangles, as in bar charts and timelines
-}
type alias Rect data xdomain ydomain =
    { x : Position data xdomain
    , y : Position data ydomain
    , cornerRadius : Maybe (FloatChannel data)
    , fill : Maybe (Fill data)
    , stroke : Maybe (Stroke data)
    , cursor : Maybe Cursor
    , href : Maybe String
    , tooltip : Maybe (TextChannel data)
    }



-- Helpers ---------------------------------------------------------------------


bar :
    PositionalChannel data xdomain
    -> FloatChannel data
    -> PositionalChannel data ydomain
    -> PositionalChannel data ydomain
    -> Rect data xdomain ydomain
bar x width y y2 =
    Rect
        (CenterExtent x width)
        (PrimarySecondary y y2)
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing


rect :
    PositionalChannel data xdomain
    -> PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> PositionalChannel data ydomain
    -> Rect data xdomain ydomain
rect x x2 y y2 =
    Rect
        (PrimarySecondary x x2)
        (PrimarySecondary y y2)
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing



-- LEGENDS ---------------------------------------------------------------------


legends : Int -> Rect data xdomain ydomain -> LegendSpec
legends ticks encoding =
    { fillColor = encoding.fill |> Maybe.andThen .fill |> Maybe.andThen (Channel.legend ticks)
    , fillOpacity = encoding.fill |> Maybe.andThen .fillOpacity |> Maybe.andThen (Channel.legend ticks)
    , strokeColor = encoding.stroke |> Maybe.andThen .stroke |> Maybe.andThen (Channel.legend ticks)
    , strokeOpacity = encoding.stroke |> Maybe.andThen .strokeOpacity |> Maybe.andThen (Channel.legend ticks)
    , strokeWidth = encoding.stroke |> Maybe.andThen .strokeWidth |> Maybe.andThen (Channel.legend ticks)
    , strokeDash = encoding.stroke |> Maybe.andThen .strokeDash |> Maybe.andThen (Channel.legend ticks)
    , angle = Nothing
    , shape = Nothing
    , cornerRadius = encoding.cornerRadius |> Maybe.andThen (Channel.legend ticks)
    , size = Nothing
    , width = Nothing
    }



-- SCENEGRAPH ------------------------------------------------------------------


{-| An intermediate representation of an encoding which allows for all
    fields to possibly be `Nothing`
-}
type alias RectInternal =
    { x : Maybe ScenegraphPosition.Position
    , y : Maybe ScenegraphPosition.Position
    , cornerRadius : Maybe Float
    , fill : Maybe Scenegraph.Fill
    , stroke : Maybe Scenegraph.Stroke
    , cursor : Maybe Cursor
    , href : Maybe String
    , tooltip : Maybe String
    }


{-| Combine `RectInternal`s with a preference for `mark2` (in practice,
    for each field in the two `RectInternal`s, only one _can_ have a `Just`
    value so the order of arguments does not matter.)
-}
combineIntermediate : RectInternal -> RectInternal -> RectInternal
combineIntermediate mark1 mark2 =
    { x = mark2.x |> Maybe.orElse mark1.x
    , y = Maybe.orElse mark1.y mark2.y
    , cornerRadius = Maybe.orElse mark1.cornerRadius mark2.cornerRadius
    , fill = Maybe.orElse mark1.fill mark2.fill
    , stroke = Maybe.orElse mark1.stroke mark2.stroke
    , cursor = Maybe.orElse mark1.cursor mark2.cursor
    , href = Maybe.orElse mark1.href mark2.href
    , tooltip = Maybe.orElse mark1.tooltip mark2.tooltip
    }


{-| Combine an `RectInternal` with a theme to populate any un-encoded fields.
-}
combineWithThemeDefaults : Theme.Rect -> RectInternal -> Maybe Mark.Rect
combineWithThemeDefaults theme markInternal =
    Maybe.map2
        (\x y ->
            { x = x
            , y = y
            , cornerRadius = Maybe.withDefault theme.cornerRadius markInternal.cornerRadius
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
containsAggregate : Rect data xdomain ydomain -> Bool
containsAggregate encoding =
    Position.containsAggregate encoding.x
        || Position.containsAggregate encoding.y
        || Maybe.maybe False Channel.isAggregate encoding.cornerRadius
        || Maybe.maybe False Fill.containsAggregate encoding.fill
        || Maybe.maybe False Stroke.containsAggregate encoding.stroke
        || Maybe.maybe False Channel.isAggregate encoding.tooltip


{-| Test for an vector field in one of the channels
-}
containsVector : Rect data xdomain ydomain -> Bool
containsVector encoding =
    Position.containsVector encoding.x
        || Position.containsVector encoding.y
        || Maybe.maybe False Channel.isVector encoding.cornerRadius
        || Maybe.maybe False Fill.containsVector encoding.fill
        || Maybe.maybe False Stroke.containsVector encoding.stroke
        || Maybe.maybe False Channel.isVector encoding.tooltip


{-| Generate a scenegraph for the encoding
-}
scenegraph :
    Theme.Rect
    -> Scale xdomain Float
    -> Scale ydomain Float
    -> List data
    -> Rect data xdomain ydomain
    -> Scenegraph
scenegraph theme xScale yScale data encoding =
    {- if the encoding contains an aggregate field we need to
       group the data by all non-aggregate fields before applying
       the aggregate fields
    -}
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
                            |> combineWithThemeDefaults theme
                )
            |> Scenegraph.Rect
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
                            |> combineWithThemeDefaults theme
                )
            |> Scenegraph.Rect
    else
        List.filterMap
            (extract xScale yScale encoding
                >> combineWithThemeDefaults theme
            )
            data
            |> Scenegraph.Rect



-- APPLY AGGREGATE, VECTOR AND SCALAR FIELDS -----------------------------------


{-| Apply each scalar channel of an encoding to a data point.
    Where a channel is not a scalar, the corresponding field in the returned
    `RectInternal` is `Nothing`.
-}
extract :
    Scale xdomain Float
    -> Scale ydomain Float
    -> Rect data xdomain ydomain
    -> data
    -> RectInternal
extract xScale yScale encoding datum =
    let
        extractMaybe getter =
            Maybe.andThen (\channel -> Channel.extract channel datum) <|
                getter encoding

        positionX =
            Position.extract xScale encoding.x datum

        positionY =
            Position.extract yScale encoding.y datum
    in
        { x = positionX
        , y = positionY
        , cornerRadius = extractMaybe .cornerRadius
        , fill = Maybe.map (\fill -> Fill.extract fill datum) encoding.fill
        , stroke = Maybe.map (\stroke -> Stroke.extract stroke datum) encoding.stroke
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
    -> Rect data xdomain ydomain
    -> List data
    -> RectInternal
extractVector xScale yScale encoding data =
    let
        extractVectorMaybe getter =
            getter encoding
                |> Maybe.andThen
                    (\channel ->
                        Channel.extractVector channel data
                            |> List.head
                            |> Maybe.join
                    )

        positionX =
            Position.extractVector xScale encoding.x data
                |> List.head
                |> Maybe.join

        positionY =
            Position.extractVector yScale encoding.y data
                |> List.head
                |> Maybe.join

        fill =
            Maybe.andThen
                (\fill -> Fill.extractVector fill data |> List.head)
                encoding.fill

        stroke =
            Maybe.andThen
                (\stroke -> Stroke.extractVector stroke data |> List.head)
                encoding.stroke
    in
        { x = positionX
        , y = positionY
        , cornerRadius = extractVectorMaybe .cornerRadius
        , fill = fill
        , stroke = stroke
        , cursor = encoding.cursor
        , href = encoding.href
        , tooltip = extractVectorMaybe .tooltip
        }


{-| Apply each aggregate channel of an encoding to a data point.
    Where a channel is not an aggregate, the corresponding field in the returned
    `RectInternal` is `Nothing`.
-}
summarize :
    Scale xdomain Float
    -> Scale ydomain Float
    -> Rect data xdomain ydomain
    -> List data
    -> RectInternal
summarize xScale yScale encoding data =
    let
        summarizeMaybe getter =
            getter encoding
                |> Maybe.andThen (\channel -> Channel.summarize channel data)

        positionX =
            Position.summarize xScale encoding.x data

        positionY =
            Position.summarize yScale encoding.y data
    in
        { x = positionX
        , y = positionY
        , cornerRadius = summarizeMaybe .cornerRadius
        , fill = Maybe.map (\fill -> Fill.summarize fill data) encoding.fill
        , stroke = Maybe.map (\stroke -> Stroke.summarize stroke data) encoding.stroke
        , cursor = encoding.cursor
        , href = encoding.href
        , tooltip = summarizeMaybe .tooltip
        }



-- Comparison and equality -----------------------------------------------------


{-| Compare the encoding at two data points
-}
compareAt : Rect data xdomain ydomain -> data -> data -> Order
compareAt encoding d1 d2 =
    let
        comp getter =
            Channel.compareMaybeAt (getter encoding) d1 d2
    in
        case Position.compareAt encoding.x d1 d2 of
            EQ ->
                case Position.compareAt encoding.y d1 d2 of
                    EQ ->
                        case comp .cornerRadius of
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

            otherwise ->
                otherwise


{-| Test for structural equality of the encoding at two data points
-}
equalAt : Rect data xdomain ydomain -> data -> data -> Bool
equalAt encoding d1 d2 =
    let
        eq getter =
            Channel.equalMaybeAt (getter encoding) d1 d2
    in
        Position.equalAt encoding.x d1 d2
            && Position.equalAt encoding.y d1 d2
            && eq .cornerRadius
            && Maybe.maybe True (\fill -> Fill.equalAt fill d1 d2) encoding.fill
            && Maybe.maybe True (\stroke -> Stroke.equalAt stroke d1 d2) encoding.stroke
            && eq .tooltip
