module Facet.Internal.Encoding.Text exposing (Text, text, scenegraph, legends)

import Facet.Internal.Channel as Channel exposing (ShapeChannel, FloatChannel, PositionalChannel, TextChannel)
import Facet.Internal.Encoding.Fill as Fill exposing (Fill)
import Facet.Internal.Encoding.Stroke as Stroke exposing (Stroke)
import Facet.Internal.Field as Field
import Facet.Internal.Legend exposing (LegendSpec)
import Facet.Maybe.Extra as Maybe
import Facet.List.Extra as List
import Facet.Scale as Scale exposing (Scale)
import Facet.Scenegraph as Scenegraph exposing (Scenegraph)
import Facet.Scenegraph.Cursor as Cursor exposing (Cursor)
import Facet.Scenegraph.Fill as Scenegraph
import Facet.Scenegraph.Stroke as Scenegraph
import Facet.Scenegraph.Mark as Mark
import Facet.Theme as Theme


type alias Text data xdomain ydomain =
    { text : TextChannel data
    , dx : Maybe Float
    , dy : Maybe Float
    , size : Maybe (FloatChannel data)
    , angle : Maybe (FloatChannel data)
    , x : PositionalChannel data xdomain
    , y : PositionalChannel data ydomain
    , fill : Maybe (Fill data)
    , stroke : Maybe (Stroke data)
    , cursor : Maybe Cursor
    , href : Maybe String
    , tooltip : Maybe (TextChannel data)
    }



-- Helpers ---------------------------------------------------------------------


text :
    TextChannel data
    -> PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Text data xdomain ydomain
text text x y =
    Text
        text
        Nothing
        Nothing
        Nothing
        Nothing
        x
        y
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing



-- LEGENDS ---------------------------------------------------------------------


legends : Int -> Text data xdomain ydomain -> LegendSpec
legends ticks encoding =
    { fillColor = encoding.fill |> Maybe.andThen .fill |> Maybe.andThen (Channel.legend ticks)
    , fillOpacity = encoding.fill |> Maybe.andThen .fillOpacity |> Maybe.andThen (Channel.legend ticks)
    , strokeColor = encoding.stroke |> Maybe.andThen .stroke |> Maybe.andThen (Channel.legend ticks)
    , strokeOpacity = encoding.stroke |> Maybe.andThen .strokeOpacity |> Maybe.andThen (Channel.legend ticks)
    , strokeWidth = encoding.stroke |> Maybe.andThen .strokeWidth |> Maybe.andThen (Channel.legend ticks)
    , strokeDash = encoding.stroke |> Maybe.andThen .strokeDash |> Maybe.andThen (Channel.legend ticks)
    , angle = encoding.angle |> Maybe.andThen (Channel.legend ticks)
    , shape = Nothing
    , cornerRadius = Nothing
    , size = encoding.size |> Maybe.andThen (Channel.legend ticks)
    , width = Nothing
    }



-- SCENEGRAPH ------------------------------------------------------------------


{-| An intermediate representation of an encoding which allows for all
    fields to possibly be `Nothing`
-}
type alias TextInternal =
    { text : Maybe String
    , size : Maybe Float
    , angle : Maybe Float
    , x : Maybe Float
    , y : Maybe Float
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
combineIntermediate : TextInternal -> TextInternal -> TextInternal
combineIntermediate mark1 mark2 =
    { x = mark2.x |> Maybe.orElse mark1.x
    , y = Maybe.orElse mark1.y mark2.y
    , text = Maybe.orElse mark1.text mark2.text
    , size = Maybe.orElse mark1.size mark2.size
    , angle = Maybe.orElse mark1.angle mark2.angle
    , fill = Maybe.orElse mark1.fill mark2.fill
    , stroke = Maybe.orElse mark1.stroke mark2.stroke
    , cursor = Maybe.orElse mark1.cursor mark2.cursor
    , href = Maybe.orElse mark1.href mark2.href
    , tooltip = Maybe.orElse mark1.tooltip mark2.tooltip
    }


{-| Combine an `SymbolInternal` with a theme to populate any un-encoded fields.
-}
combineWithThemeDefaults : Theme.Text -> Maybe Float -> Maybe Float -> TextInternal -> Maybe Mark.Text
combineWithThemeDefaults theme dx dy markInternal =
    Maybe.map3
        (\text x y ->
            let
                themeFont =
                    theme.font

                font =
                    Maybe.map (\size -> { themeFont | fontSize = size }) markInternal.size
                        |> Maybe.withDefault themeFont
            in
                { text = text
                , align = theme.align
                , baseline = theme.baseline
                , direction = theme.direction
                , dx = Maybe.withDefault 0 dx
                , dy = Maybe.withDefault 0 dy
                , elipsis = theme.elipsis
                , font = font
                , angle = Maybe.withDefault theme.angle markInternal.angle
                , radius = 0
                , theta = 0
                , x = x
                , y = y
                , fill = Maybe.withDefault theme.fill markInternal.fill
                , stroke = Maybe.withDefault theme.stroke markInternal.stroke
                , cursor = Maybe.withDefault theme.cursor markInternal.cursor
                , href = markInternal.href
                , tooltip = markInternal.tooltip
                }
        )
        markInternal.text
        markInternal.x
        markInternal.y


{-| Test for an aggregate field in one of the channels
-}
containsAggregate : Text data xdomain ydomain -> Bool
containsAggregate encoding =
    Channel.isAggregate encoding.text
        || Maybe.maybe False Channel.isAggregate encoding.size
        || Maybe.maybe False Channel.isAggregate encoding.angle
        || Channel.isAggregate encoding.x
        || Channel.isAggregate encoding.y
        || Maybe.maybe False Fill.containsAggregate encoding.fill
        || Maybe.maybe False Stroke.containsAggregate encoding.stroke
        || Maybe.maybe False Channel.isAggregate encoding.tooltip


containsVector : Text data xdomain ydomain -> Bool
containsVector encoding =
    Channel.isVector encoding.text
        || Maybe.maybe False Channel.isVector encoding.size
        || Maybe.maybe False Channel.isVector encoding.angle
        || Channel.isVector encoding.x
        || Channel.isVector encoding.y
        || Maybe.maybe False Fill.containsVector encoding.fill
        || Maybe.maybe False Stroke.containsVector encoding.stroke
        || Maybe.maybe False Channel.isVector encoding.tooltip


scenegraph :
    Theme.Text
    -> Scale xdomain Float
    -> Scale ydomain Float
    -> List data
    -> Text data xdomain ydomain
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
                            |> combineWithThemeDefaults theme encoding.dx encoding.dy
                )
            |> Scenegraph.Text
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
                            |> combineWithThemeDefaults theme encoding.dx encoding.dy
                )
            |> Scenegraph.Text
    else
        List.filterMap
            (extract xScale yScale encoding
                >> combineWithThemeDefaults theme encoding.dx encoding.dy
            )
            data
            |> Scenegraph.Text



--
--
-- APPLY AGGREGATE, VECTOR AND SCALAR FIELDS -----------------------------------


{-| Apply each scalar channel of an encoding to a data point.
    Where a channel is not a scalar, the corresponding field in the returned
    `SymbolInternal` is `Nothing`.
-}
extract :
    Scale xdomain Float
    -> Scale ydomain Float
    -> Text data xdomain ydomain
    -> data
    -> TextInternal
extract xScale yScale encoding datum =
    let
        extractMaybe getter =
            Maybe.andThen (\channel -> Channel.extract channel datum) <|
                getter encoding

        positionX =
            Field.extract encoding.x.field datum
                |> Maybe.andThen (Scale.scale xScale)

        positionY =
            Field.extract encoding.y.field datum
                |> Maybe.andThen (Scale.scale yScale)
    in
        { x = positionX
        , y = positionY
        , size = extractMaybe .size
        , angle = extractMaybe .angle
        , text = Channel.extract encoding.text datum
        , fill = Maybe.map (\fill -> Fill.extract fill datum) encoding.fill
        , stroke = Maybe.map (\stroke -> Stroke.extract stroke datum) encoding.stroke
        , cursor = encoding.cursor
        , href = encoding.href
        , tooltip = extractMaybe .tooltip
        }


{-| Apply each vector channel of an encoding to a data point.
    Where a channel is not a vector, the corresponding field in the returned
    `SymbolInternal` is `Nothing`.
-}
extractVector :
    Scale xdomain Float
    -> Scale ydomain Float
    -> Text data xdomain ydomain
    -> List data
    -> TextInternal
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

        extractVector channel =
            Channel.extractVector (channel encoding) data
                |> List.head
                |> Maybe.join

        positionX =
            Field.extractVector encoding.x.field data
                |> List.head
                |> Maybe.join
                |> Maybe.andThen
                    (Scale.scale xScale)

        positionY =
            Field.extractVector encoding.y.field data
                |> List.head
                |> Maybe.join
                |> Maybe.andThen
                    (Scale.scale yScale)

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
        , size = extractVectorMaybe .size
        , angle = extractVectorMaybe .angle
        , text = extractVector .text
        , fill = fill
        , stroke = stroke
        , cursor = encoding.cursor
        , href = encoding.href
        , tooltip = extractVectorMaybe .tooltip
        }


{-| Apply each aggregate channel of an encoding to a data point.
    Where a channel is not an aggregate, the corresponding field in the returned
    `SymbolInternal` is `Nothing`.
-}
summarize :
    Scale xdomain Float
    -> Scale ydomain Float
    -> Text data xdomain ydomain
    -> List data
    -> TextInternal
summarize xScale yScale encoding data =
    let
        summarizeMaybe getter =
            getter encoding
                |> Maybe.andThen (\channel -> Channel.summarize channel data)

        positionX =
            Field.summarize encoding.x.field data
                |> Maybe.andThen (Scale.scale xScale)

        positionY =
            Field.summarize encoding.y.field data
                |> Maybe.andThen (Scale.scale yScale)
    in
        { x = positionX
        , y = positionY
        , size = summarizeMaybe .size
        , angle = summarizeMaybe .angle
        , text = Channel.summarize encoding.text data
        , fill = Maybe.map (\fill -> Fill.summarize fill data) encoding.fill
        , stroke = Maybe.map (\stroke -> Stroke.summarize stroke data) encoding.stroke
        , cursor = encoding.cursor
        , href = encoding.href
        , tooltip = summarizeMaybe .tooltip
        }


{-| Structural comparison of `Stroke` encoding domain evaluated at two data poinst
-}
compareAt : Text data xdomain ydomain -> data -> data -> Order
compareAt encoding d1 d2 =
    let
        comp getter =
            Channel.compareMaybeAt (getter encoding) d1 d2
    in
        case Channel.compareAt encoding.x d1 d2 of
            EQ ->
                case Channel.compareAt encoding.y d1 d2 of
                    EQ ->
                        case Channel.compareAt encoding.text d1 d2 of
                            EQ ->
                                case comp .size of
                                    EQ ->
                                        case comp .angle of
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


equalAt : Text data xdomain ydomain -> data -> data -> Bool
equalAt encoding d1 d2 =
    let
        eq getter =
            Channel.equalMaybeAt (getter encoding) d1 d2
    in
        Channel.equalAt encoding.x d1 d2
            && Channel.equalAt encoding.y d1 d2
            && Channel.equalAt encoding.text d1 d2
            && Maybe.maybe True (\fill -> Fill.equalAt fill d1 d2) encoding.fill
            && Maybe.maybe True (\stroke -> Stroke.equalAt stroke d1 d2) encoding.stroke
            && eq .size
            && eq .angle
            && eq .tooltip
