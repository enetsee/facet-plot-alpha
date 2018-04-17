module Facet.Internal.Encoding.Line exposing (Line, line, scenegraph, legends)

import Facet.Internal.Encoding.Stroke as Stroke exposing (Stroke)
import Facet.Internal.Channel as Channel exposing (TextChannel, PositionalChannel)
import Facet.Internal.Field as Field
import Facet.Internal.Legend exposing (LegendSpec)
import Facet.List.Extra as List
import Facet.Maybe.Extra as Maybe
import Facet.Scale as Scale exposing (Scale)
import Facet.Scenegraph as Scenegraph exposing (Scenegraph)
import Facet.Scenegraph.Cursor as Cursor exposing (Cursor)
import Facet.Scenegraph.Stroke as Scenegraph
import Facet.Scenegraph.Interpolate exposing (Interpolate(..))
import Facet.Scenegraph.Mark as Mark exposing (Behaviour(..), Orientation(..))
import Facet.Scenegraph.Mark as Mark
import Facet.Theme as Theme


type alias Line data xdomain ydomain =
    { x : PositionalChannel data xdomain
    , y : PositionalChannel data ydomain
    , interpolate : Interpolate
    , behaviour : Mark.Behaviour
    , stroke : Maybe (Stroke data)
    , cursor : Maybe Cursor
    , href : Maybe String
    , tooltip : Maybe (TextChannel data)
    }



-- Helpers ---------------------------------------------------------------------


line :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Interpolate
    -> Behaviour
    -> Line data xdomain ydomain
line x y interpolate behaviour =
    Line
        x
        y
        interpolate
        behaviour
        Nothing
        Nothing
        Nothing
        Nothing



-- LEGENDS ---------------------------------------------------------------------


legends : Int -> Line data xdomain ydomain -> LegendSpec
legends ticks encoding =
    { fillColor = Nothing
    , fillOpacity = Nothing
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
type alias LineInternal =
    { x : Maybe (List (Maybe Float))
    , y : Maybe (List (Maybe Float))
    , stroke : Maybe Scenegraph.Stroke
    , cursor : Maybe Cursor
    , href : Maybe String
    , tooltip : Maybe String
    }


{-| Combine `LineInternal`s with a preference for `mark2` (in practice,
    for each field in the two `RectInternal`s, only one _can_ have a `Just`
    value so the order of arguments does not matter.)
-}
combineIntermediate : LineInternal -> LineInternal -> LineInternal
combineIntermediate mark1 mark2 =
    { x = Maybe.orElse mark1.x mark2.x
    , y = Maybe.orElse mark1.y mark2.y
    , stroke = Maybe.orElse mark1.stroke mark2.stroke
    , cursor = Maybe.orElse mark1.cursor mark2.cursor
    , href = Maybe.orElse mark1.href mark2.href
    , tooltip = Maybe.orElse mark1.tooltip mark2.tooltip
    }


{-| Combine an `LineInternal` with required fields from the encoding
    and a theme to populate any un-encoded fields.
-}
combineWithThemeDefaults : Theme.Line -> Interpolate -> Behaviour -> LineInternal -> Maybe Mark.Line
combineWithThemeDefaults theme interpolate behaviour markInternal =
    Maybe.map2
        (\x y ->
            { x = x
            , y = y
            , interpolate = interpolate
            , behaviour = behaviour
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
containsAggregate : Line data xdomain ydomain -> Bool
containsAggregate encoding =
    Channel.isAggregate encoding.x
        || Channel.isAggregate encoding.y
        || Maybe.maybe False Stroke.containsAggregate encoding.stroke
        || Maybe.maybe False Channel.isAggregate encoding.tooltip


containsVector : Line data xdomain ydomain -> Bool
containsVector encoding =
    Channel.isVector encoding.x
        || Channel.isVector encoding.y
        || Maybe.maybe False Stroke.containsVector encoding.stroke
        || Maybe.maybe False Channel.isVector encoding.tooltip



{- Generate a scenegraph for the encoding -}


scenegraph :
    Theme.Line
    -> Scale xdomain Float
    -> Scale ydomain Float
    -> List data
    -> Line data xdomain ydomain
    -> Scenegraph
scenegraph theme xScale yScale data encoding =
    if containsVector encoding then
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
                            |> combineIntermediate agg
                            |> combineIntermediate vector
                            |> combineWithThemeDefaults theme encoding.interpolate encoding.behaviour
                )
            |> Scenegraph.Line
    else if containsAggregate encoding then
        data
            |> List.groupBy (compareAt encoding)
                (extract xScale yScale encoding)
            |> List.filterMap
                (\( mark, data ) ->
                    let
                        agg =
                            summarize xScale yScale encoding data
                    in
                        mark
                            |> combineIntermediate agg
                            |> combineWithThemeDefaults theme encoding.interpolate encoding.behaviour
                )
            |> Scenegraph.Line
    else
        List.filterMap
            (extract xScale yScale encoding
                >> combineWithThemeDefaults theme encoding.interpolate encoding.behaviour
            )
            data
            |> Scenegraph.Line



-- APPLY AGGREGATE, VECTOR AND SCALAR FIELDS -----------------------------------


{-| Apply each scalar channel of an encoding to a data point.
    Where a channel is not a scalar, the corresponding field in the returned
    `LineInternal` is `Nothing`.
-}
extract :
    Scale xdomain Float
    -> Scale ydomain Float
    -> Line data xdomain ydomain
    -> data
    -> LineInternal
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

        stroke =
            Maybe.map (\stroke -> Stroke.extract stroke datum) encoding.stroke
    in
        { x = positionX
        , y = positionY
        , stroke = stroke
        , cursor = encoding.cursor
        , href = encoding.href
        , tooltip = extractMaybe .tooltip
        }


{-| Apply each vector channel of an encoding to a data point.
    Where a channel is not a vector, the corresponding field in the returned
    `LineInternal` is `Nothing`.
-}
extractVector :
    Scale xdomain Float
    -> Scale ydomain Float
    -> Line data xdomain ydomain
    -> List data
    -> LineInternal
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
        , stroke = stroke
        , cursor = encoding.cursor
        , href = encoding.href
        , tooltip = tooltip
        }


{-| Apply each aggregate channel of an encoding to a data point.
    Where a channel is not an aggregate, the corresponding field in the returned
    `LineInternal` is `Nothing`.
-}
summarize :
    Scale xdomain Float
    -> Scale ydomain Float
    -> Line data xdomain ydomain
    -> List data
    -> LineInternal
summarize xScale yScale encoding data =
    let
        summarizeMaybe getter =
            getter encoding
                |> Maybe.andThen (\channel -> Channel.summarize channel data)

        positionX =
            Field.summarize encoding.x.field data
                |> Maybe.andThen (Scale.scale xScale >> Maybe.map (Just >> List.singleton))

        positionY =
            Field.summarize encoding.x.field data
                |> Maybe.andThen (Scale.scale xScale >> Maybe.map (Just >> List.singleton))
    in
        { x = positionX
        , y = positionY
        , stroke = Maybe.map (\stroke -> Stroke.summarize stroke data) encoding.stroke
        , cursor = encoding.cursor
        , href = encoding.href
        , tooltip = summarizeMaybe .tooltip
        }


{-| Structural comparison of encoding domain evaluated at two data poinst
-}
compareAt : Line data xdomain ydomain -> data -> data -> Order
compareAt encoding d1 d2 =
    let
        comp getter =
            Channel.compareMaybeAt (getter encoding) d1 d2
    in
        case Channel.compareAt encoding.x d1 d2 of
            EQ ->
                case Channel.compareAt encoding.y d1 d2 of
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


equalAt : Line data xdomain ydomain -> data -> data -> Bool
equalAt encoding d1 d2 =
    let
        eq getter =
            Channel.equalMaybeAt (getter encoding) d1 d2
    in
        Channel.equalAt encoding.x d1 d2
            && Channel.equalAt encoding.y d1 d2
            && Maybe.maybe True (\stroke -> Stroke.equalAt stroke d1 d2) encoding.stroke
            && eq .tooltip
