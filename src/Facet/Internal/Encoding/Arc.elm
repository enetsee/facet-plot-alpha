module Facet.Internal.Encoding.Arc exposing (Arc, arc, scenegraph, legends)

import Facet.Internal.Channel as Channel exposing (ShapeChannel, FloatChannel, PositionalChannel, TextChannel)
import Facet.Internal.Encoding.Fill as Fill exposing (Fill)
import Facet.Internal.Encoding.Stroke as Stroke exposing (Stroke)
import Facet.Internal.Field as Field
import Facet.Internal.Legend exposing (Legend, LegendSpec)
import Facet.List.Extra as List
import Facet.Maybe.Extra as Maybe
import Facet.Scale as Scale exposing (Scale)
import Facet.Scenegraph as Scenegraph exposing (Scenegraph)
import Facet.Scenegraph.Cursor as Cursor exposing (Cursor)
import Facet.Scenegraph.Fill as Scenegraph
import Facet.Scenegraph.Stroke as Scenegraph
import Facet.Scenegraph.Mark as Mark exposing (Behaviour(..), Orientation(..))
import Facet.Theme as Theme


type alias Arc data xdomain ydomain =
    { x : PositionalChannel data xdomain
    , y : PositionalChannel data ydomain
    , startAngle : FloatChannel data
    , endAngle : FloatChannel data
    , padAngle : Maybe (FloatChannel data)
    , outerRadius : FloatChannel data
    , innerRadius : Maybe (FloatChannel data)
    , cornerRadius : Maybe (FloatChannel data)
    , fill : Maybe (Fill data)
    , stroke : Maybe (Stroke data)
    , cursor : Maybe Cursor
    , href : Maybe String
    , tooltip : Maybe (TextChannel data)
    }



-- Helpers ---------------------------------------------------------------------


arc :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> FloatChannel data
    -> FloatChannel data
    -> FloatChannel data
    -> Arc data xdomain ydomain
arc x y startAngle endAngle outerRadius =
    { x = x
    , y = y
    , startAngle = startAngle
    , endAngle = endAngle
    , padAngle = Nothing
    , outerRadius = outerRadius
    , innerRadius = Nothing
    , cornerRadius = Nothing
    , fill = Nothing
    , stroke = Nothing
    , cursor = Nothing
    , href = Nothing
    , tooltip = Nothing
    }



-- LEGENDS ---------------------------------------------------------------------


legends : Int -> Arc data xdomain ydomain -> LegendSpec
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
    , size = encoding.outerRadius |> Channel.legend ticks
    , width = Nothing
    }



-- SCENEGRAPH ------------------------------------------------------------------


{-| An intermediate representation of an encoding which allows for all
    fields to possibly be `Nothing`
-}
type alias ArcInternal =
    { x : Maybe Float
    , y : Maybe Float
    , startAngle : Maybe Float
    , endAngle : Maybe Float
    , padAngle : Maybe Float
    , innerRadius : Maybe Float
    , outerRadius : Maybe Float
    , cornerRadius : Maybe Float
    , fill : Maybe Scenegraph.Fill
    , stroke : Maybe Scenegraph.Stroke
    , cursor : Maybe Cursor
    , href : Maybe String
    , tooltip : Maybe String
    }


{-| Combine `AreaInternal`s with a preference for `mark2` (in practice,
    for each field in the two `RectInternal`s, only one _can_ have a `Just`
    value so the order of arguments does not matter.)
-}
combineIntermediate : ArcInternal -> ArcInternal -> ArcInternal
combineIntermediate mark1 mark2 =
    { x = mark2.x |> Maybe.orElse mark1.x
    , y = Maybe.orElse mark1.y mark2.y
    , startAngle = Maybe.orElse mark1.startAngle mark2.startAngle
    , endAngle = Maybe.orElse mark1.endAngle mark2.endAngle
    , padAngle = Maybe.orElse mark1.padAngle mark2.padAngle
    , innerRadius = Maybe.orElse mark1.innerRadius mark2.innerRadius
    , outerRadius = Maybe.orElse mark1.outerRadius mark2.outerRadius
    , cornerRadius = Maybe.orElse mark1.cornerRadius mark2.cornerRadius
    , fill = Maybe.orElse mark1.fill mark2.fill
    , stroke = Maybe.orElse mark1.stroke mark2.stroke
    , cursor = Maybe.orElse mark1.cursor mark2.cursor
    , href = Maybe.orElse mark1.href mark2.href
    , tooltip = Maybe.orElse mark1.tooltip mark2.tooltip
    }


{-| Combine an `AreaInternal` with required fields from the encoding
    and a theme to populate any un-encoded fields.
-}
combineWithThemeDefaults :
    Theme.Arc
    -> ArcInternal
    -> Maybe Mark.Arc
combineWithThemeDefaults theme markInternal =
    Maybe.map5
        (\x y startAngle endAngle outerRadius ->
            { x = x
            , y = y
            , startAngle = startAngle
            , endAngle = endAngle
            , padAngle = Maybe.withDefault 0 markInternal.padAngle
            , outerRadius = outerRadius
            , innerRadius = Maybe.withDefault 0 markInternal.innerRadius
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
        markInternal.startAngle
        markInternal.endAngle
        markInternal.outerRadius


{-| Test for an aggregate field in one of the channels
-}
containsAggregate : Arc data xdomain ydomain -> Bool
containsAggregate encoding =
    Channel.isAggregate encoding.x
        || Channel.isAggregate encoding.y
        || Channel.isAggregate encoding.startAngle
        || Channel.isAggregate encoding.endAngle
        || Channel.isAggregate encoding.outerRadius
        || Maybe.maybe False Channel.isAggregate encoding.innerRadius
        || Maybe.maybe False Channel.isAggregate encoding.cornerRadius
        || Maybe.maybe False Fill.containsAggregate encoding.fill
        || Maybe.maybe False Stroke.containsAggregate encoding.stroke
        || Maybe.maybe False Channel.isAggregate encoding.tooltip


containsVector : Arc data xdomain ydomain -> Bool
containsVector encoding =
    Channel.isVector encoding.x
        || Channel.isVector encoding.y
        || Channel.isVector encoding.startAngle
        || Channel.isVector encoding.endAngle
        || Channel.isVector encoding.outerRadius
        || Maybe.maybe False Channel.isVector encoding.innerRadius
        || Maybe.maybe False Channel.isVector encoding.cornerRadius
        || Maybe.maybe False Fill.containsVector encoding.fill
        || Maybe.maybe False Stroke.containsVector encoding.stroke
        || Maybe.maybe False Channel.isVector encoding.tooltip



{- Generate a scenegraph for the encoding -}


scenegraph :
    Theme.Arc
    -> Scale xdomain Float
    -> Scale ydomain Float
    -> List data
    -> Arc data xdomain ydomain
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
                            |> combineWithThemeDefaults theme
                )
            |> Scenegraph.Arc
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
            |> Scenegraph.Arc
    else
        List.filterMap
            (extract xScale yScale encoding
                >> combineWithThemeDefaults theme
            )
            data
            |> Scenegraph.Arc



-- APPLY AGGREGATE, VECTOR AND SCALAR FIELDS -----------------------------------


{-| Apply each scalar channel of an encoding to a data point.
    Where a channel is not a scalar, the corresponding field in the returned
    `RectInternal` is `Nothing`.
-}
extract :
    Scale xdomain Float
    -> Scale ydomain Float
    -> Arc data xdomain ydomain
    -> data
    -> ArcInternal
extract xScale yScale encoding datum =
    let
        extractMaybe getter =
            getter encoding
                |> Maybe.andThen (\channel -> Channel.extract channel datum)

        extract getter =
            Channel.extract (getter encoding) datum

        positionX =
            Field.extract encoding.x.field datum
                |> Maybe.andThen (Scale.scale xScale)

        positionY =
            Field.extract encoding.y.field datum
                |> Maybe.andThen (Scale.scale yScale)

        fill =
            Maybe.map (\fill -> Fill.extract fill datum) encoding.fill

        stroke =
            Maybe.map (\stroke -> Stroke.extract stroke datum) encoding.stroke
    in
        { x = positionX
        , y = positionY
        , startAngle = extract .startAngle
        , endAngle = extract .endAngle
        , padAngle = extractMaybe .padAngle
        , outerRadius = extract .outerRadius
        , innerRadius = extractMaybe .innerRadius
        , cornerRadius = extractMaybe .cornerRadius
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
    -> Arc data xdomain ydomain
    -> List data
    -> ArcInternal
extractVector xScale yScale encoding data =
    let
        x =
            Field.extractVector encoding.x.field data
                |> List.head
                |> Maybe.join
                |> Maybe.andThen (Scale.scale xScale)

        y =
            Field.extractVector encoding.x.field data
                |> List.head
                |> Maybe.join
                |> Maybe.andThen (Scale.scale xScale)

        fill =
            Maybe.andThen
                (\fill -> Fill.extractVector fill data |> List.head)
                encoding.fill

        stroke =
            Maybe.andThen
                (\stroke -> Stroke.extractVector stroke data |> List.head)
                encoding.stroke

        extractVector getter =
            Channel.extractVector (getter encoding) data
                |> List.head
                |> Maybe.join

        extractVectorMaybe getter =
            getter encoding
                |> Maybe.andThen
                    (\channel ->
                        Channel.extractVector channel data
                            |> List.head
                            >> Maybe.join
                    )
    in
        { x = x
        , y = y
        , startAngle = extractVector .startAngle
        , endAngle = extractVector .endAngle
        , padAngle = extractVectorMaybe .padAngle
        , innerRadius = extractVectorMaybe .innerRadius
        , outerRadius = extractVector .outerRadius
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
    -> Arc data xdomain ydomain
    -> List data
    -> ArcInternal
summarize xScale yScale encoding data =
    let
        summarizeMaybe getter =
            getter encoding
                |> Maybe.andThen (\channel -> Channel.summarize channel data)

        summarize getter =
            Channel.summarize (getter encoding) data

        positionX =
            Field.summarize encoding.x.field data
                |> Maybe.andThen (Scale.scale xScale)

        positionY =
            Field.summarize encoding.y.field data
                |> Maybe.andThen (Scale.scale yScale)
    in
        { x = positionX
        , y = positionY
        , startAngle = summarize .startAngle
        , endAngle = summarize .endAngle
        , padAngle = summarizeMaybe .padAngle
        , innerRadius = summarizeMaybe .innerRadius
        , outerRadius = summarize .outerRadius
        , cornerRadius = summarizeMaybe .cornerRadius
        , fill = Maybe.map (\fill -> Fill.summarize fill data) encoding.fill
        , stroke = Maybe.map (\stroke -> Stroke.summarize stroke data) encoding.stroke
        , cursor = encoding.cursor
        , href = encoding.href
        , tooltip = summarizeMaybe .tooltip
        }


{-| Structural comparison of encoding domain evaluated at two data poinst
-}
compareAt : Arc data xdomain ydomain -> data -> data -> Order
compareAt encoding d1 d2 =
    let
        comp getter =
            Channel.compareMaybeAt (getter encoding) d1 d2
    in
        case Channel.compareAt encoding.x d1 d2 of
            EQ ->
                case Channel.compareAt encoding.y d1 d2 of
                    EQ ->
                        case Channel.compareAt encoding.startAngle d1 d2 of
                            EQ ->
                                case Channel.compareAt encoding.endAngle d1 d2 of
                                    EQ ->
                                        case Channel.compareAt encoding.outerRadius d1 d2 of
                                            EQ ->
                                                case comp .innerRadius of
                                                    EQ ->
                                                        case comp .padAngle of
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


equalAt : Arc data xdomain ydomain -> data -> data -> Bool
equalAt encoding d1 d2 =
    let
        eq getter =
            Channel.equalMaybeAt (getter encoding) d1 d2
    in
        Channel.equalAt encoding.x d1 d2
            && Channel.equalAt encoding.y d1 d2
            && Channel.equalAt encoding.startAngle d1 d2
            && Channel.equalAt encoding.endAngle d1 d2
            && Channel.equalAt encoding.outerRadius d1 d2
            && eq .innerRadius
            && eq .padAngle
            && eq .cornerRadius
            && Maybe.maybe True (\fill -> Fill.equalAt fill d1 d2) encoding.fill
            && Maybe.maybe True (\stroke -> Stroke.equalAt stroke d1 d2) encoding.stroke
            && eq .tooltip
