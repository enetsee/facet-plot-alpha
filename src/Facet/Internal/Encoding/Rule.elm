module Facet.Internal.Encoding.Rule exposing (Rule, rule, scenegraph, legends)

import Facet.Internal.Channel as Channel exposing (TextChannel, PositionalChannel)
import Facet.Internal.Encoding.Position as Position exposing (Position(..))
import Facet.Internal.Encoding.Stroke as Stroke exposing (Stroke)
import Facet.Internal.Legend exposing (LegendSpec)
import Facet.List.Extra as List
import Facet.Maybe.Extra as Maybe
import Facet.Scale as Scale exposing (Scale)
import Facet.Scenegraph as Scenegraph exposing (Scenegraph)
import Facet.Scenegraph.Cursor as Cursor exposing (Cursor)
import Facet.Scenegraph.Position as Scenegraph
import Facet.Scenegraph.Stroke as Scenegraph
import Facet.Scenegraph.Mark as Mark
import Facet.Theme as Theme


type alias Rule data xdomain ydomain =
    { x : Position data xdomain
    , y : Position data ydomain
    , stroke : Maybe (Stroke data)
    , cursor : Maybe Cursor
    , href : Maybe String
    , tooltip : Maybe (TextChannel data)
    }



-- Helpers ---------------------------------------------------------------------


rule :
    PositionalChannel data xdomain
    -> PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> PositionalChannel data ydomain
    -> Rule data xdomain ydomain
rule x x2 y y2 =
    let
        xpos =
            PrimarySecondary x x2

        ypos =
            PrimarySecondary y y2
    in
        Rule xpos ypos Nothing Nothing Nothing Nothing



-- LEGENDS ---------------------------------------------------------------------


legends : Int -> Rule data xdomain ydomain -> LegendSpec
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
type alias RuleInternal =
    { x : Maybe Scenegraph.Position
    , y : Maybe Scenegraph.Position
    , stroke : Maybe Scenegraph.Stroke
    , cursor : Maybe Cursor
    , href : Maybe String
    , tooltip : Maybe String
    }


{-| Combine `RuleInternal`s with a preference for `mark2` (in practice,
    for each field in the two `RuleInternal`s, only one _can_ have a `Just`
    value so the order of arguments does not matter.)
-}
combineIntermediate : RuleInternal -> RuleInternal -> RuleInternal
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
combineWithThemeDefaults : Theme.Rule -> RuleInternal -> Maybe Mark.Rule
combineWithThemeDefaults theme markInternal =
    Maybe.map2
        (\x y ->
            { x = x
            , y = y
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
containsAggregate : Rule data xdomain ydomain -> Bool
containsAggregate encoding =
    Position.containsAggregate encoding.x
        || Position.containsAggregate encoding.y
        || Maybe.maybe False Stroke.containsAggregate encoding.stroke
        || Maybe.maybe False Channel.isAggregate encoding.tooltip


containsVector : Rule data xdomain ydomain -> Bool
containsVector encoding =
    Position.containsVector encoding.x
        || Position.containsVector encoding.y
        || Maybe.maybe False Stroke.containsVector encoding.stroke
        || Maybe.maybe False Channel.isVector encoding.tooltip



{- Generate a scenegraph for the encoding -}


scenegraph :
    Theme.Rule
    -> Scale xdomain Float
    -> Scale ydomain Float
    -> List data
    -> Rule data xdomain ydomain
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
            |> Scenegraph.Rule
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
            |> Scenegraph.Rule
    else
        List.filterMap
            (extract xScale yScale encoding
                >> combineWithThemeDefaults theme
            )
            data
            |> Scenegraph.Rule



-- APPLY AGGREGATE, VECTOR AND SCALAR FIELDS -----------------------------------


{-| Apply each scalar channel of an encoding to a data point.
    Where a channel is not a scalar, the corresponding field in the returned
    `RuleInternal` is `Nothing`.
-}
extract :
    Scale xdomain Float
    -> Scale ydomain Float
    -> Rule data xdomain ydomain
    -> data
    -> RuleInternal
extract xScale yScale encoding datum =
    let
        extractMaybe getter =
            getter encoding
                |> Maybe.andThen (\channel -> Channel.extract channel datum)

        positionX =
            Position.extract xScale encoding.x datum

        positionY =
            Position.extract yScale encoding.y datum

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
    `RuleInternal` is `Nothing`.
-}
extractVector :
    Scale xdomain Float
    -> Scale ydomain Float
    -> Rule data xdomain ydomain
    -> List data
    -> RuleInternal
extractVector xScale yScale encoding data =
    let
        xs =
            Position.extractVector xScale encoding.x data
                |> List.head
                |> Maybe.join

        ys =
            Position.extractVector yScale encoding.y data
                |> List.head
                |> Maybe.join

        ( x, y ) =
            Maybe.map2 (\x y -> ( Just x, Just y )) xs ys
                |> Maybe.withDefault ( Nothing, Nothing )

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
        { x = x
        , y = y
        , stroke = stroke
        , cursor = encoding.cursor
        , href = encoding.href
        , tooltip = tooltip
        }


{-| Apply each aggregate channel of an encoding to a data point.
    Where a channel is not an aggregate, the corresponding field in the returned
    `RuleInternal` is `Nothing`.
-}
summarize :
    Scale xdomain Float
    -> Scale ydomain Float
    -> Rule data xdomain ydomain
    -> List data
    -> RuleInternal
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
        , stroke = Maybe.map (\stroke -> Stroke.summarize stroke data) encoding.stroke
        , cursor = encoding.cursor
        , href = encoding.href
        , tooltip = summarizeMaybe .tooltip
        }


{-| Structural comparison of encoding domain evaluated at two data poinst
-}
compareAt : Rule data xdomain ydomain -> data -> data -> Order
compareAt encoding d1 d2 =
    let
        comp getter =
            Channel.compareMaybeAt (getter encoding) d1 d2
    in
        case Position.compareAt encoding.x d1 d2 of
            EQ ->
                case Position.compareAt encoding.y d1 d2 of
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


equalAt : Rule data xdomain ydomain -> data -> data -> Bool
equalAt encoding d1 d2 =
    let
        eq getter =
            Channel.equalMaybeAt (getter encoding) d1 d2
    in
        Position.equalAt encoding.x d1 d2
            && Position.equalAt encoding.y d1 d2
            && Maybe.maybe True (\stroke -> Stroke.equalAt stroke d1 d2) encoding.stroke
            && eq .tooltip
