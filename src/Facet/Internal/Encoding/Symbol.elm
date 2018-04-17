module Facet.Internal.Encoding.Symbol
    exposing
        ( Symbol
        , symbol
        , point
        , arrow
        , cross
        , square
        , diamond
        , triangleUp
        , triangleDown
        , triangleLeft
        , triangleRight
        , shape
        , scenegraph
        , legends
        )

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
import Facet.Scenegraph.Shape as Scenegraph
import Facet.Scenegraph.Mark as Mark
import Facet.Theme as Theme
import Path.LowLevel exposing (SubPath)


{-| Plotting symbols, including circles, squares and other shapes
-}
type alias Symbol data xdomain ydomain =
    { shape : Maybe (ShapeChannel data)
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


{-| Construct a `Symbol` encoding providing an explicit `ShapeChannel`.
-}
symbol :
    ShapeChannel data
    -> PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Symbol data xdomain ydomain
symbol shape x y =
    Symbol
        (Just shape)
        Nothing
        Nothing
        x
        y
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing


{-| Construct a `Symbol` encoding defaulting the `shape` to whatever is set
    in the theme applied
-}
point :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Symbol data xdomain ydomain
point x y =
    Symbol
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


arrow :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Symbol data xdomain ydomain
arrow x y =
    symbolHelper x y Scenegraph.Arrow


cross :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Symbol data xdomain ydomain
cross x y =
    symbolHelper x y Scenegraph.Cross


square :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Symbol data xdomain ydomain
square x y =
    symbolHelper x y Scenegraph.Square


diamond :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Symbol data xdomain ydomain
diamond x y =
    symbolHelper x y Scenegraph.Diamond


triangleUp :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Symbol data xdomain ydomain
triangleUp x y =
    symbolHelper x y Scenegraph.TriangleUp


triangleDown :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Symbol data xdomain ydomain
triangleDown x y =
    symbolHelper x y Scenegraph.TriangleDown


triangleLeft :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Symbol data xdomain ydomain
triangleLeft x y =
    symbolHelper x y Scenegraph.TriangleLeft


triangleRight :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Symbol data xdomain ydomain
triangleRight x y =
    symbolHelper x y Scenegraph.TriangleRight


shape :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> SubPath
    -> Symbol data xdomain ydomain
shape x y subpath =
    symbolHelper x y <| Scenegraph.Custom subpath


symbolHelper :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Scenegraph.Shape
    -> Symbol data xdomain ydomain
symbolHelper x y shape =
    symbol
        (Channel.shape toString (Scale.constant shape) (Field.constant 0))
        x
        y



-- LEGENDS ---------------------------------------------------------------------


legends : Int -> Symbol data xdomain ydomain -> LegendSpec
legends ticks encoding =
    { fillColor = encoding.fill |> Maybe.andThen .fill |> Maybe.andThen (Channel.legend ticks)
    , fillOpacity = encoding.fill |> Maybe.andThen .fillOpacity |> Maybe.andThen (Channel.legend ticks)
    , strokeColor = encoding.stroke |> Maybe.andThen .stroke |> Maybe.andThen (Channel.legend ticks)
    , strokeOpacity = encoding.stroke |> Maybe.andThen .strokeOpacity |> Maybe.andThen (Channel.legend ticks)
    , strokeWidth = encoding.stroke |> Maybe.andThen .strokeWidth |> Maybe.andThen (Channel.legend ticks)
    , strokeDash = encoding.stroke |> Maybe.andThen .strokeDash |> Maybe.andThen (Channel.legend ticks)
    , angle = encoding.angle |> Maybe.andThen (Channel.legend ticks)
    , shape = encoding.shape |> Maybe.andThen (Channel.legend ticks)
    , cornerRadius = Nothing
    , size = encoding.size |> Maybe.andThen (Channel.legend ticks)
    , width = Nothing
    }



-- SCENEGRAPH ------------------------------------------------------------------


{-| An intermediate representation of an encoding which allows for all
    fields to possibly be `Nothing`
-}
type alias SymbolInternal =
    { shape : Maybe Scenegraph.Shape
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
combineIntermediate : SymbolInternal -> SymbolInternal -> SymbolInternal
combineIntermediate mark1 mark2 =
    { x = mark2.x |> Maybe.orElse mark1.x
    , y = Maybe.orElse mark1.y mark2.y
    , shape = Maybe.orElse mark1.shape mark2.shape
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
combineWithThemeDefaults : Theme.Symbol -> SymbolInternal -> Maybe Mark.Symbol
combineWithThemeDefaults theme markInternal =
    Maybe.map2
        (\x y ->
            { x = x
            , y = y
            , shape = Maybe.withDefault theme.shape markInternal.shape
            , size = Maybe.withDefault theme.size markInternal.size
            , angle = Maybe.withDefault theme.angle markInternal.angle
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
containsAggregate : Symbol data xdomain ydomain -> Bool
containsAggregate encoding =
    Maybe.maybe False Channel.isAggregate encoding.shape
        || Maybe.maybe False Channel.isAggregate encoding.size
        || Maybe.maybe False Channel.isAggregate encoding.angle
        || Channel.isAggregate encoding.x
        || Channel.isAggregate encoding.y
        || Maybe.maybe False Fill.containsAggregate encoding.fill
        || Maybe.maybe False Stroke.containsAggregate encoding.stroke
        || Maybe.maybe False Channel.isAggregate encoding.tooltip


containsVector : Symbol data xdomain ydomain -> Bool
containsVector encoding =
    Maybe.maybe False Channel.isVector encoding.shape
        || Maybe.maybe False Channel.isVector encoding.size
        || Maybe.maybe False Channel.isVector encoding.angle
        || Channel.isVector encoding.x
        || Channel.isVector encoding.y
        || Maybe.maybe False Fill.containsVector encoding.fill
        || Maybe.maybe False Stroke.containsVector encoding.stroke
        || Maybe.maybe False Channel.isVector encoding.tooltip


scenegraph :
    Theme.Symbol
    -> Scale xdomain Float
    -> Scale ydomain Float
    -> List data
    -> Symbol data xdomain ydomain
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
            |> Scenegraph.Symbol
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
            |> Scenegraph.Symbol
    else
        List.filterMap
            (extract xScale yScale encoding
                >> combineWithThemeDefaults theme
            )
            data
            |> Scenegraph.Symbol



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
    -> Symbol data xdomain ydomain
    -> data
    -> SymbolInternal
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
        , shape = extractMaybe .shape
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
    -> Symbol data xdomain ydomain
    -> List data
    -> SymbolInternal
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
        , shape = extractVectorMaybe .shape
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
    -> Symbol data xdomain ydomain
    -> List data
    -> SymbolInternal
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
        , shape = summarizeMaybe .shape
        , fill = Maybe.map (\fill -> Fill.summarize fill data) encoding.fill
        , stroke = Maybe.map (\stroke -> Stroke.summarize stroke data) encoding.stroke
        , cursor = encoding.cursor
        , href = encoding.href
        , tooltip = summarizeMaybe .tooltip
        }


{-| Structural comparison of `Stroke` encoding domain evaluated at two data poinst
-}
compareAt : Symbol data xdomain ydomain -> data -> data -> Order
compareAt encoding d1 d2 =
    let
        comp getter =
            Channel.compareMaybeAt (getter encoding) d1 d2
    in
        case Channel.compareAt encoding.x d1 d2 of
            EQ ->
                case Channel.compareAt encoding.y d1 d2 of
                    EQ ->
                        case comp .shape of
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


equalAt : Symbol data xdomain ydomain -> data -> data -> Bool
equalAt encoding d1 d2 =
    let
        eq getter =
            Channel.equalMaybeAt (getter encoding) d1 d2
    in
        Channel.equalAt encoding.x d1 d2
            && Channel.equalAt encoding.y d1 d2
            && Maybe.maybe True (\fill -> Fill.equalAt fill d1 d2) encoding.fill
            && Maybe.maybe True (\stroke -> Stroke.equalAt stroke d1 d2) encoding.stroke
            && eq .shape
            && eq .size
            && eq .angle
            && eq .tooltip
