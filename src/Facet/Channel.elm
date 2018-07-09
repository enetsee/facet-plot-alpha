module Facet.Channel
    exposing
        ( PositionalChannel
        , ChannelWithLegend
        , AngleChannel
        , ColorChannel
        , FloatChannel
        , IntChannel
        , ShapeChannel
        , TextChannel
        , StrokeDashChannel
        , positionalCompareWith
        , positional
        , channelCompareWith
        , channel
        , angleCompareWith
        , angle
        , colorCompareWith
        , color
        , floatCompareWith
        , float
        , intCompareWith
        , int
        , shapeCompareWith
        , shape
        , textCompareWith
        , text
        , strokeDashCompareWith
        , strokeDash
        )

{-|

# Channel

A `Channel` is a means representing data as some attribute of a visual mark.

## Positional Channel
@docs PositionalChannel, positional, positionalCompareWith

@docs ChannelWithLegend, channel, channelCompareWith

## Angle Channel
@docs AngleChannel, angle, angleCompareWith

## Color Channel
@docs ColorChannel, color, colorCompareWith

## Float Channel
@docs FloatChannel, float, floatCompareWith

## Int Channel
@docs IntChannel, int, intCompareWith

## Shape Channel
@docs ShapeChannel, shape, shapeCompareWith

## Text Channel
@docs TextChannel , text, textCompareWith

## Stroke-dash Channel
@docs StrokeDashChannel, strokeDash, strokeDashCompareWith

-}

import Color exposing (Color)
import Facet.Internal.Channel as Channel
import Facet.Field exposing (Field)
import Facet.Scale exposing (Scale)
import Facet.Scenegraph.Shape exposing (Shape)
import Facet.Scenegraph.Stroke exposing (StrokeDash)


{-| A `PositionalChannel` is used to associate a data value with a position on either
the x- or y-axis.
-}
type alias PositionalChannel data domain =
    Channel.PositionalChannel data domain


{-| -}
positional :
    Field data comparableDomain
    -> PositionalChannel data comparableDomain
positional field =
    Channel.positional field


{-| -}
positionalCompareWith :
    { a
        | compareWith : domain -> domain -> Order
        , field : Field data domain
    }
    -> PositionalChannel data domain
positionalCompareWith { compareWith, field } =
    Channel.positionalCompareWith compareWith field


{-| -}
type alias ChannelWithLegend data range =
    Channel.ChannelWithLegend data range


{-| -}
channelCompareWith :
    { a
        | compareWith : domain -> domain -> Order
        , formatDomain : domain -> String
        , scale : Scale domain range
        , field : Field data domain
    }
    -> ChannelWithLegend data range
channelCompareWith { compareWith, formatDomain, scale, field } =
    Channel.channelCompareWith compareWith formatDomain scale field


{-| -}
channel :
    { a
        | formatDomain : comparableDomain -> String
        , scale : Scale comparableDomain range
        , field : Field data comparableDomain
    }
    -> ChannelWithLegend data range
channel { formatDomain, scale, field } =
    Channel.channel formatDomain scale field


{-| An `AngleChannel` is used to encode data as the rotation of a visual mark.
-}
type alias AngleChannel data =
    Channel.AngleChannel data


{-| -}
angle :
    { a
        | formatDomain : comparableDomain -> String
        , scale : Scale comparableDomain Float
        , field : Field data comparableDomain
    }
    -> AngleChannel data
angle { formatDomain, scale, field } =
    Channel.angle formatDomain scale field


{-| -}
angleCompareWith :
    { a
        | compareWith : domain -> domain -> Order
        , formatDomain : domain -> String
        , scale : Scale domain Float
        , field : Field data domain
    }
    -> AngleChannel data
angleCompareWith { compareWith, formatDomain, scale, field } =
    Channel.angleCompareWith compareWith formatDomain scale field


{-| A `ColorChannel` is used to encode data as either the fill color or stroke color
of a visual mark.
-}
type alias ColorChannel data =
    Channel.ColorChannel data


{-| -}
color :
    { a
        | formatDomain : comparableDomain -> String
        , scale : Scale comparableDomain Color
        , field : Field data comparableDomain
    }
    -> ColorChannel data
color { formatDomain, scale, field } =
    Channel.color formatDomain scale field


{-| -}
colorCompareWith :
    { a
        | compareWith : domain -> domain -> Order
        , formatDomain : domain -> String
        , scale : Scale domain Color
        , field : Field data domain
    }
    -> ColorChannel data
colorCompareWith { compareWith, formatDomain, scale, field } =
    Channel.colorCompareWith compareWith formatDomain scale field


{-| A `FloatChannel` is used to encode data as some non-positional numeric attribute
of a visual mark e.g. stroke width, size, font size.
-}
type alias FloatChannel data =
    Channel.FloatChannel data


{-| -}
float :
    { a
        | formatDomain : comparableDomain -> String
        , scale : Scale comparableDomain Float
        , field : Field data comparableDomain
    }
    -> FloatChannel data
float { formatDomain, scale, field } =
    Channel.float formatDomain scale field


{-| -}
floatCompareWith :
    { a
        | compareWith : domain -> domain -> Order
        , formatDomain : domain -> String
        , scale : Scale domain Float
        , field : Field data domain
    }
    -> FloatChannel data
floatCompareWith { compareWith, formatDomain, scale, field } =
    Channel.floatCompareWith compareWith formatDomain scale field


{-| A `IntChannel` is used to encode data as some non-positional numeric attribute
of a visual mark e.g. stroke width, size, font size.
-}
type alias IntChannel data =
    Channel.IntChannel data


{-| -}
int :
    { a
        | formatDomain : comparableDomain -> String
        , scale : Scale comparableDomain Int
        , field : Field data comparableDomain
    }
    -> IntChannel data
int { formatDomain, scale, field } =
    Channel.int formatDomain scale field


{-| -}
intCompareWith :
    { a
        | compareWith : domain -> domain -> Order
        , formatDomain : domain -> String
        , scale : Scale domain Int
        , field : Field data domain
    }
    -> IntChannel data
intCompareWith { compareWith, formatDomain, scale, field } =
    Channel.intCompareWith compareWith formatDomain scale field


{-| A `ShapeChannel` is used to encode data as the shape used in a `Symbol` visual
mark.
-}
type alias ShapeChannel data =
    Channel.ShapeChannel data


{-| -}
shape :
    { a
        | formatDomain : comparableDomain -> String
        , scale : Scale comparableDomain Shape
        , field : Field data comparableDomain
    }
    -> ShapeChannel data
shape { formatDomain, scale, field } =
    Channel.shape formatDomain scale field


{-| -}
shapeCompareWith :
    { a
        | compareWith : domain -> domain -> Order
        , formatDomain : domain -> String
        , scale : Scale domain Shape
        , field : Field data domain
    }
    -> ShapeChannel data
shapeCompareWith { compareWith, formatDomain, scale, field } =
    Channel.shapeCompareWith compareWith formatDomain scale field


{-| A `StrokeDashChannel` is used to encode data as the stroke dash array
and (optional) stroke dash offset of a visual mark.
-}
type alias StrokeDashChannel data =
    Channel.StrokeDashChannel data


{-| -}
strokeDash :
    { a
        | formatDomain : comparableDomain -> String
        , scale : Scale comparableDomain StrokeDash
        , field : Field data comparableDomain
    }
    -> StrokeDashChannel data
strokeDash { formatDomain, scale, field } =
    Channel.strokeDash formatDomain scale field


{-| -}
strokeDashCompareWith :
    { a
        | compareWith : domain -> domain -> Order
        , formatDomain : domain -> String
        , scale : Scale domain StrokeDash
        , field : Field data domain
    }
    -> StrokeDashChannel data
strokeDashCompareWith { compareWith, formatDomain, scale, field } =
    Channel.strokeDashCompareWith compareWith formatDomain scale field


{-| A `TextChannel` is used to encode data as the text of a `Text` mark or as the
tooltip of any visual mark.
-}
type alias TextChannel data =
    Channel.TextChannel data


{-| -}
text :
    { a
        | formatDomain : comparableDomain -> String
        , scale : Scale comparableDomain String
        , field : Field data comparableDomain
    }
    -> TextChannel data
text { formatDomain, scale, field } =
    Channel.text formatDomain scale field


{-| -}
textCompareWith :
    { a
        | compareWith : domain -> domain -> Order
        , formatDomain : domain -> String
        , scale : Scale domain String
        , field : Field data domain
    }
    -> TextChannel data
textCompareWith { compareWith, formatDomain, scale, field } =
    Channel.textCompareWith compareWith formatDomain scale field
