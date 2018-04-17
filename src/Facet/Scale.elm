module Facet.Scale
    exposing
        ( Scale
        , scale
        , unscale
        , ticks
        , legend
        , OutsideDomain
        , clamp
        , discard
        , allow
        , linear
        , linearNice
        , log10
        , naturalLog
        , sqrt
        , rgb
        , rgbBasis
        , hsl
        , tableau10
        , category10
        , band
        , customBand
        , continuous
        , sequential
        , ordinal
        , customOrdinal
        , constant
        )

{-|
@docs Scale,scale, unscale, ticks, legend

@docs constant, sequential, continuous, ordinal, customOrdinal

@docs  OutsideDomain, clamp, discard, allow

@docs linear, linearNice , log10, naturalLog, sqrt

@docs band, customBand

@docs rgb, rgbBasis, hsl

@docs tableau10, category10
-}

import Color exposing (Color)
import Scale.Config as Config
import Scale.Continuous as Continuous exposing (Continuous)
import Scale.Continuous.Linear as Linear
import Scale.Continuous.Log as Log
import Scale.Continuous.Power as Power
import Scale.Ordinal as Ordinal exposing (Ordinal)
import Scale.Ordinal.Comparable as Ordinal
import Scale.Ordinal.Custom as Ordinal
import Scale.Ordinal.Band as Band
import Scale.Sequential as Sequential exposing (Sequential)
import Scale.Sequential.RGB as RGB
import Scale.Sequential.RGBBasis as RGBBasis
import Scale.Sequential.HSL as HSL


{-| A scale provides a means of mapping between values of type _domain_ to
    values of type _range_.
-}
type Scale domain range
    = Continuous (Continuous domain range)
    | Ordinal (Ordinal domain range)
    | Sequential (Sequential domain range)
    | Constant range


{-| -}
scale : Scale domain range -> domain -> Maybe range
scale scale datum =
    case scale of
        Continuous continuous ->
            Continuous.scale continuous datum

        Ordinal ordinal ->
            Ordinal.scale ordinal datum

        Sequential sequential ->
            Sequential.scale sequential datum

        Constant range ->
            Just range


{-| -}
unscale : Scale domain Float -> Float -> Maybe domain
unscale scale datum =
    case scale of
        Continuous continuous ->
            Continuous.unscale continuous datum

        _ ->
            Nothing


{-| -}
ticks : Scale domain range -> Int -> List domain
ticks scale count =
    case scale of
        Continuous continuous ->
            Continuous.ticks continuous count

        Ordinal ordinal ->
            Ordinal.ticks ordinal count

        Sequential sequential ->
            Sequential.ticks sequential count

        Constant _ ->
            []


{-| -}
legend : Scale domain range -> Int -> List ( domain, range )
legend forScale count =
    ticks forScale count
        |> List.filterMap
            (\x ->
                scale forScale x
                    |> Maybe.map (\y -> ( x, y ))
            )



-- constructors ----------------------------------------------------------------


{-| -}
constant : a -> Scale domain a
constant range =
    Constant range


{-| -}
continuous :
    { a
        | compareDomain : domain -> domain -> Order
        , deinterpolateDomain : ( domain, domain ) -> domain -> Float
        , domain : ( domain, domain )
        , interpolateRange : ( range, range ) -> Float -> range
        , outsideDomain : OutsideDomain
        , range : ( range, range )
        , reinterpolateDomain : ( domain, domain ) -> range -> domain
        , ticks : ( domain, domain ) -> Int -> List domain
    }
    -> Scale domain range
continuous =
    Continuous << Continuous.continuous


{-| -}
ordinal :
    Ordinal.Comparable a comparableDomain range
    -> Scale comparableDomain range
ordinal =
    Ordinal << Ordinal.comparable


{-| -}
customOrdinal : Ordinal.Custom a domain range -> Scale domain range
customOrdinal =
    Ordinal << Ordinal.custom


{-| -}
sequential :
    { a
        | compareDomain : domain -> domain -> Order
        , deinterpolateDomain : ( domain, domain ) -> domain -> Float
        , domain : ( domain, domain )
        , interpolator : Float -> range
        , outsideDomain : OutsideDomain
        , ticks : ( domain, domain ) -> Int -> List domain
    }
    -> Scale domain range
sequential =
    Sequential << Sequential.sequential



-- continuous scales -----------------------------------------------------------


{-| -}
linear : ( Float, Float ) -> ( Float, Float ) -> Scale Float Float
linear domain range =
    { domain = domain, range = range, outsideDomain = discard }
        -- |> Linear.niceDomain 10
        |>
            Linear.linear
        |> Continuous


{-| -}
linearNice : Int -> ( Float, Float ) -> ( Float, Float ) -> Scale Float Float
linearNice n domain range =
    { domain = domain, range = range, outsideDomain = discard }
        |> Linear.niceDomain n
        |> Linear.linear
        |> Continuous


{-| -}
log10 : ( Float, Float ) -> ( Float, Float ) -> Scale Float Float
log10 domain range =
    { base = 10.0, domain = domain, range = range, outsideDomain = discard }
        |> Log.log
        |> Continuous


{-| -}
naturalLog : ( Float, Float ) -> ( Float, Float ) -> Scale Float Float
naturalLog domain range =
    { base = Basics.e, domain = domain, range = range, outsideDomain = discard }
        |> Log.niceDomain
        |> Log.log
        |> Continuous


{-| -}
sqrt : ( Float, Float ) -> ( Float, Float ) -> Scale Float Float
sqrt domain range =
    { exponent = 0.5, domain = domain, range = range, outsideDomain = discard }
        |> Power.niceDomain 10
        |> Power.power
        |> Continuous



-- Band scales -----------------------------------------------------------------


{-| -}
customBand : List domain -> ( Float, Float ) -> Scale domain Float
customBand domain range =
    { domain = domain, range = range, config = Nothing }
        |> Band.customBand
        |> Ordinal


{-| -}
band : List comparable -> ( Float, Float ) -> Scale comparable Float
band domain range =
    { domain = domain, range = range, config = Nothing }
        |> Band.band
        |> Ordinal



-- Sequential color scales -----------------------------------------------------


{-| -}
rgb :
    ( Float, Float )
    -> ( Color, Color )
    -> Scale Float Color
rgb domain range =
    { domain = domain, range = range, gamma = Nothing, outsideDomain = discard }
        |> RGB.rgb
        |> Sequential


{-| -}
rgbBasis : Bool -> ( Float, Float ) -> List Color -> Scale Float Color
rgbBasis closed domain range =
    { domain = domain, range = range, closed = closed, outsideDomain = discard }
        |> RGBBasis.rgbBasis
        |> Sequential


{-| -}
hsl : Bool -> ( Float, Float ) -> ( Color, Color ) -> Scale Float Color
hsl long domain range =
    { domain = domain, range = range, long = long, outsideDomain = discard }
        |> HSL.hsl
        |> Sequential



-- Ordinal color scales --------------------------------------------------------


{-| -}
tableau10 : List comparable -> Scale comparable Color
tableau10 domain =
    { domain = domain
    , range = tableau10Colors
    }
        |> Ordinal.comparable
        |> Ordinal


tableau10Colors : List Color
tableau10Colors =
    [ Color.rgb 76 120 168
    , Color.rgb 245 133 24
    , Color.rgb 228 87 86
    , Color.rgb 114 183 178
    , Color.rgb 84 162 75
    , Color.rgb 238 202 59
    , Color.rgb 178 121 162
    , Color.rgb 255 157 166
    , Color.rgb 157 117 93
    , Color.rgb 186 176 172
    ]


{-| -}
category10 : List comparable -> Scale comparable Color
category10 domain =
    { domain = domain
    , range = category10Colors
    }
        |> Ordinal.comparable
        |> Ordinal


category10Colors : List Color
category10Colors =
    [ Color.rgb 31 119 180
    , Color.rgb 255 127 14
    , Color.rgb 44 160 44
    , Color.rgb 214 39 40
    , Color.rgb 148 103 189
    , Color.rgb 140 86 75
    , Color.rgb 227 119 194
    , Color.rgb 17 127 127
    , Color.rgb 188 189 34
    , Color.rgb 23 190 207
    ]



-- RE-EXPORTS ------------------------------------------------------------------


{-| -}
type alias OutsideDomain =
    Config.OutsideDomain


{-| -}
clamp : Config.OutsideDomain
clamp =
    Config.Clamp


{-| -}
discard : Config.OutsideDomain
discard =
    Config.Discard


{-| -}
allow : Config.OutsideDomain
allow =
    Config.Allow
