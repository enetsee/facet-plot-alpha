module Facet.Internal.Encoding.Stroke
    exposing
        ( Stroke
        , containsAggregate
        , containsVector
        , extract
        , extractVector
        , summarize
        , equalAt
        , compareAt
        , empty
        )

{-|
@docs Stroke, containsAggregate, extractOrElse, summarizeOrElse
-}

import Color exposing (Color)
import Facet.Internal.Channel as Channel exposing (ColorChannel, FloatChannel, StrokeDashChannel)
import Facet.Maybe.Extra as Maybe
import Facet.Scenegraph.Stroke as Mark


{-| -}
type alias Stroke data =
    { stroke : Maybe (ColorChannel data)
    , strokeOpacity : Maybe (FloatChannel data)
    , strokeWidth : Maybe (FloatChannel data)
    , strokeDash : Maybe (StrokeDashChannel data)
    }


empty : Stroke data
empty =
    Stroke Nothing Nothing Nothing Nothing


equalAt : Stroke data -> data -> data -> Bool
equalAt stroke d1 d2 =
    let
        eq getter =
            Channel.equalMaybeAt (getter stroke) d1 d2
    in
        eq .stroke
            && eq .strokeOpacity
            && eq .strokeWidth
            && eq .strokeDash


{-| Structural comparison of `Stroke` encoding domain evaluated at two data poinst
-}
compareAt : Stroke data -> data -> data -> Order
compareAt stroke d1 d2 =
    let
        comp getter =
            Channel.compareMaybeAt (getter stroke) d1 d2
    in
        case comp .stroke of
            EQ ->
                case comp .strokeOpacity of
                    EQ ->
                        case comp .strokeWidth of
                            EQ ->
                                comp .strokeDash

                            otherwise ->
                                otherwise

                    otherwise ->
                        otherwise

            otherwise ->
                otherwise


{-| -}
containsAggregate :
    Stroke data
    -> Bool
containsAggregate stroke =
    Maybe.maybe False Channel.isAggregate stroke.stroke
        || Maybe.maybe False Channel.isAggregate stroke.strokeOpacity
        || Maybe.maybe False Channel.isAggregate stroke.strokeWidth
        || Maybe.maybe False Channel.isAggregate stroke.strokeDash


{-| -}
containsVector :
    Stroke data
    -> Bool
containsVector stroke =
    Maybe.maybe False Channel.isVector stroke.stroke
        || Maybe.maybe False Channel.isVector stroke.strokeOpacity
        || Maybe.maybe False Channel.isVector stroke.strokeWidth
        || Maybe.maybe False Channel.isVector stroke.strokeDash


extract : Stroke data -> data -> Mark.Stroke
extract stroke datum =
    let
        f =
            Maybe.andThen (\ch -> Channel.extract ch datum)
    in
        { stroke = f stroke.stroke
        , strokeOpacity = f stroke.strokeOpacity
        , strokeWidth = f stroke.strokeWidth
        , strokeDash = f stroke.strokeDash
        , strokeLineCap = Nothing
        , strokeLineJoin = Nothing
        }


extractVector : Stroke data -> List data -> List Mark.Stroke
extractVector stroke data =
    let
        f getter =
            getter stroke
                |> Maybe.map (\ch -> Channel.extractVector ch data)
                |> Maybe.withDefault []

        strokes =
            f .stroke

        opacities =
            f .strokeOpacity

        widths =
            f .strokeWidth

        -- List(Maybe(List Float))
        dashes =
            f .strokeDash
    in
        extractVectorHelper [] strokes opacities widths dashes


extractVectorHelper :
    List Mark.Stroke
    -> List (Maybe Color)
    -> List (Maybe Float)
    -> List (Maybe Float)
    -> List (Maybe Mark.StrokeDash)
    -> List Mark.Stroke
extractVectorHelper accu strokes opacities widths dashes =
    case ( strokes, opacities, widths, dashes ) of
        ( [], [], [], [] ) ->
            List.reverse accu

        _ ->
            let
                ( nextStroke, restStroke ) =
                    headTail strokes

                ( nextOpacity, restOpacity ) =
                    headTail opacities

                ( nextWidth, restWidth ) =
                    headTail widths

                ( nextDash, restDash ) =
                    headTail dashes

                stroke =
                    Mark.Stroke nextStroke nextOpacity nextWidth Nothing nextDash Nothing
            in
                extractVectorHelper (stroke :: accu)
                    restStroke
                    restOpacity
                    restWidth
                    restDash


headTail : List (Maybe a) -> ( Maybe a, List b )
headTail xs =
    case xs of
        [] ->
            ( Nothing, [] )

        x :: xs ->
            ( x, [] )


summarize : Stroke data -> List data -> Mark.Stroke
summarize stroke data =
    let
        f =
            Maybe.andThen (\ch -> Channel.summarize ch data)
    in
        { stroke = f stroke.stroke
        , strokeOpacity = f stroke.strokeOpacity
        , strokeWidth = f stroke.strokeWidth
        , strokeLineCap = Nothing
        , strokeDash = f stroke.strokeDash
        , strokeLineJoin = Nothing
        }
