module Facet.Internal.Encoding.Trail exposing (Trail, trail, scenegraph, legends)

import Facet.Internal.Encoding.Fill as Fill exposing (Fill)
import Facet.Internal.Channel as Channel exposing (TextChannel, FloatChannel, PositionalChannel)
import Facet.Internal.Field as Field
import Facet.Internal.Legend exposing (LegendSpec)
import Facet.List.Extra as List
import Facet.Maybe.Extra as Maybe
import Facet.Scale as Scale exposing (Scale)
import Facet.Scenegraph as Scenegraph exposing (Scenegraph)
import Facet.Scenegraph.Cursor as Cursor exposing (Cursor)
import Facet.Scenegraph.Fill as Scenegraph
import Facet.Scenegraph.Mark as Mark exposing (Behaviour)
import Facet.Theme as Theme


type alias Trail data xdomain ydomain =
    { x : PositionalChannel data xdomain
    , y : PositionalChannel data ydomain
    , width : FloatChannel data
    , behaviour : Behaviour
    , fill : Maybe (Fill data)
    , cursor : Maybe Cursor
    , href : Maybe String
    , tooltip : Maybe (TextChannel data)
    }



-- Helpers ---------------------------------------------------------------------


trail :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> FloatChannel data
    -> Behaviour
    -> Trail data xdomain ydomain
trail x y width behaviour =
    Trail x y width behaviour Nothing Nothing Nothing Nothing



-- LEGENDS ---------------------------------------------------------------------


legends : Int -> Trail data xdomain ydomain -> LegendSpec
legends ticks encoding =
    { fillColor = encoding.fill |> Maybe.andThen .fill |> Maybe.andThen (Channel.legend ticks)
    , fillOpacity = encoding.fill |> Maybe.andThen .fillOpacity |> Maybe.andThen (Channel.legend ticks)
    , strokeColor = Nothing
    , strokeOpacity = Nothing
    , strokeWidth = Nothing
    , strokeDash = Nothing
    , angle = Nothing
    , shape = Nothing
    , cornerRadius = Nothing
    , width = encoding.width |> Channel.legend ticks
    , size = Nothing
    }



-- SCENEGRAPH ------------------------------------------------------------------


{-| An intermediate representation of an encoding which allows for all
    fields to possibly be `Nothing`
-}
type alias TrailInternal =
    { x : Maybe (List (Maybe Float))
    , y : Maybe (List (Maybe Float))
    , width : Maybe (List (Maybe Float))
    , fill : Maybe Scenegraph.Fill
    , cursor : Maybe Cursor
    , href : Maybe String
    , tooltip : Maybe String
    }


{-| Combine `LineInternal`s with a preference for `mark2` (in practice,
    for each field in the two `RectInternal`s, only one _can_ have a `Just`
    value so the order of arguments does not matter.)
-}
combineIntermediate : TrailInternal -> TrailInternal -> TrailInternal
combineIntermediate mark1 mark2 =
    { x = Maybe.orElse mark1.x mark2.x
    , y = Maybe.orElse mark1.y mark2.y
    , width = Maybe.orElse mark1.width mark2.width
    , fill = Maybe.orElse mark1.fill mark2.fill
    , cursor = Maybe.orElse mark1.cursor mark2.cursor
    , href = Maybe.orElse mark1.href mark2.href
    , tooltip = Maybe.orElse mark1.tooltip mark2.tooltip
    }


{-| Combine an `TrailInternal` with required fields from the encoding
    and a theme to populate any un-encoded fields.
-}
combineWithThemeDefaults : Theme.Trail -> Behaviour -> TrailInternal -> Maybe Mark.Trail
combineWithThemeDefaults theme behaviour markInternal =
    Maybe.map3
        (\x y width ->
            { x = x
            , y = y
            , width = width
            , behaviour = behaviour
            , fill = Maybe.withDefault theme.fill markInternal.fill
            , cursor = Maybe.withDefault theme.cursor markInternal.cursor
            , href = markInternal.href
            , tooltip = markInternal.tooltip
            }
        )
        markInternal.x
        markInternal.y
        markInternal.width


{-| Test for an aggregate field in one of the channels
-}
containsAggregate : Trail data xdomain ydomain -> Bool
containsAggregate encoding =
    Channel.isAggregate encoding.x
        || Channel.isAggregate encoding.y
        || Channel.isAggregate encoding.width
        || Maybe.maybe False Fill.containsAggregate encoding.fill
        || Maybe.maybe False Channel.isAggregate encoding.tooltip


containsVector : Trail data xdomain ydomain -> Bool
containsVector encoding =
    Channel.isVector encoding.x
        || Channel.isVector encoding.y
        || Channel.isVector encoding.width
        || Maybe.maybe False Fill.containsVector encoding.fill
        || Maybe.maybe False Channel.isVector encoding.tooltip



{- Generate a scenegraph for the encoding -}


scenegraph :
    Theme.Trail
    -> Scale xdomain Float
    -> Scale ydomain Float
    -> List data
    -> Trail data xdomain ydomain
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
                            |> combineWithThemeDefaults theme encoding.behaviour
                )
            |> Scenegraph.Trail
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
                            |> combineWithThemeDefaults theme encoding.behaviour
                )
            |> Scenegraph.Trail
    else
        List.filterMap
            (extract xScale yScale encoding
                >> combineWithThemeDefaults theme encoding.behaviour
            )
            data
            |> Scenegraph.Trail



-- APPLY AGGREGATE, VECTOR AND SCALAR FIELDS -----------------------------------


{-| Apply each scalar channel of an encoding to a data point.
    Where a channel is not a scalar, the corresponding field in the returned
    `TrailInternal` is `Nothing`.
-}
extract :
    Scale xdomain Float
    -> Scale ydomain Float
    -> Trail data xdomain ydomain
    -> data
    -> TrailInternal
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

        width =
            Channel.extract encoding.width datum
                |> Maybe.map (Just >> List.singleton)

        fill =
            Maybe.map (\fill -> Fill.extract fill datum) encoding.fill
    in
        { x = positionX
        , y = positionY
        , width = width
        , fill = fill
        , cursor = encoding.cursor
        , href = encoding.href
        , tooltip = extractMaybe .tooltip
        }


{-| Apply each vector channel of an encoding to a data point.
    Where a channel is not a vector, the corresponding field in the returned
    `TrailInternal` is `Nothing`.
-}
extractVector :
    Scale xdomain Float
    -> Scale ydomain Float
    -> Trail data xdomain ydomain
    -> List data
    -> TrailInternal
extractVector xScale yScale encoding data =
    let
        xs =
            case Field.extractVector encoding.x.field data |> List.map (Maybe.andThen (Scale.scale xScale)) of
                [] ->
                    Nothing

                xs ->
                    Just xs

        ys =
            case Field.extractVector encoding.y.field data |> List.map (Maybe.andThen (Scale.scale yScale)) of
                [] ->
                    Nothing

                ys ->
                    Just ys

        ws =
            case Channel.extractVector encoding.width data of
                [] ->
                    Nothing

                ws ->
                    Just ws

        fill =
            Maybe.andThen
                (\fill -> Fill.extractVector fill data |> List.head)
                encoding.fill

        tooltip =
            encoding.tooltip
                |> Maybe.andThen
                    (\channel ->
                        Channel.extractVector channel data
                            |> List.head
                            >> Maybe.join
                    )
    in
        { x = xs
        , y = ys
        , width = ws
        , fill = fill
        , cursor = encoding.cursor
        , href = encoding.href
        , tooltip = tooltip
        }


{-| Apply each aggregate channel of an encoding to a data point.
    Where a channel is not an aggregate, the corresponding field in the returned
    `TrailInternal` is `Nothing`.
-}
summarize :
    Scale xdomain Float
    -> Scale ydomain Float
    -> Trail data xdomain ydomain
    -> List data
    -> TrailInternal
summarize xScale yScale encoding data =
    let
        summarizeMaybe getter =
            getter encoding
                |> Maybe.andThen (\channel -> Channel.summarize channel data)

        x =
            Field.summarize encoding.x.field data
                |> Maybe.andThen (Scale.scale xScale >> Maybe.map (Just >> List.singleton))

        y =
            Field.summarize encoding.x.field data
                |> Maybe.andThen (Scale.scale xScale >> Maybe.map (Just >> List.singleton))

        width =
            Channel.summarize encoding.width data
                |> Maybe.map (Just >> List.singleton)

        fill =
            Maybe.map (\fill -> Fill.summarize fill data) encoding.fill
    in
        { x = x
        , y = y
        , width = width
        , fill = fill
        , cursor = encoding.cursor
        , href = encoding.href
        , tooltip = summarizeMaybe .tooltip
        }


{-| Structural comparison of encoding domain evaluated at two data poinst
-}
compareAt : Trail data xdomain ydomain -> data -> data -> Order
compareAt encoding d1 d2 =
    let
        comp getter =
            Channel.compareMaybeAt (getter encoding) d1 d2
    in
        case Channel.compareAt encoding.x d1 d2 of
            EQ ->
                case Channel.compareAt encoding.y d1 d2 of
                    EQ ->
                        case Channel.compareAt encoding.width d1 d2 of
                            EQ ->
                                case Maybe.maybe EQ (\fill -> Fill.compareAt fill d1 d2) encoding.fill of
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


equalAt : Trail data xdomain ydomain -> data -> data -> Bool
equalAt encoding d1 d2 =
    let
        eq getter =
            Channel.equalMaybeAt (getter encoding) d1 d2
    in
        Channel.equalAt encoding.x d1 d2
            && Channel.equalAt encoding.y d1 d2
            && Channel.equalAt encoding.width d1 d2
            && Maybe.maybe True (\fill -> Fill.equalAt fill d1 d2) encoding.fill
            && eq .tooltip
