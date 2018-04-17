module Facet.Axis
    exposing
        ( Axis
        , continuousX
        , continuousY
        , discreteX
        , discreteY
        , bandX
        , bandY
        , customBandX
        , customBandY
        , linearX
        , linearY
        , log10X
        , log10Y
        , sqrtX
        , sqrtY
        , continuousScale
        , ordinalScale
        , continuousDomain
        , ordinalDomain
        , ticks
        , labelFormat
        , labelAngle
        , orientTop
        , orientBottom
        , orientLeft
        , orientRight
        , Vertical
        , Horizontal
        )

{-|
@docs Axis, Horizontal , Vertical

@docs continuousX, continuousY

@docs linearX, linearY, log10X, log10Y, sqrtX, sqrtY

@docs discreteX, discreteY

@docs bandX, customBandX , bandY, customBandY

@docs continuousScale, ordinalScale

@docs continuousDomain, ordinalDomain

@docs ticks, labelFormat,orientTop, orientBottom, orientLeft,orientRight, labelAngle

-}

import Facet.Internal.Axis as Axis
import Facet.Scale exposing (Scale)


{-| An `Axis` is a special type of `Legend` for positional channels
-}
type alias Axis orientation domain =
    Axis.Axis orientation domain


{-| -}
type alias Horizontal =
    Axis.Horizontal


{-| -}
type alias Vertical =
    Axis.Vertical


{-| Create an x-axis with a continuous domain, defined by an upper and lower
    limit.
-}
continuousX :
    Maybe String
    -> (( xdomain, xdomain ) -> ( Float, Float ) -> Scale xdomain Float)
    -> Axis Vertical xdomain
continuousX title scale =
    Axis.continuousX title scale


{-| Create a y-axis with a continuos domain, defined by an upper and lower
    limit.
-}
continuousY :
    Maybe String
    -> (( ydomain, ydomain ) -> ( Float, Float ) -> Scale ydomain Float)
    -> Axis Horizontal ydomain
continuousY title scale =
    Axis.continuousY title scale


{-| Create an x-axis with a discrete domain, defined by the list of elements
    in the domain.
-}
discreteX :
    Maybe String
    -> (List xdomain -> ( Float, Float ) -> Scale xdomain Float)
    -> Axis Vertical xdomain
discreteX title scale =
    Axis.discreteX title scale


{-| Create an x-axis with a discrete domain, explicitly providing a function
    to construct an ordinal scale
-}
discreteY :
    Maybe String
    -> (List ydomain -> ( Float, Float ) -> Scale ydomain Float)
    -> Axis Horizontal ydomain
discreteY title scale =
    Axis.discreteY title scale


{-| Create an x-axis with a band scale from a comparable domain
-}
bandX : Maybe String -> Axis.Axis Vertical comparableDomain
bandX title =
    Axis.bandX title


{-| Create a y-axis with a band scale from a comparable domain
-}
bandY : Maybe String -> Axis.Axis Horizontal comparableDomain
bandY title =
    Axis.bandY title


{-| Create an x-axis with a band scale from an arbitrary domain
-}
customBandX : Maybe String -> Axis.Axis Vertical anyDomain
customBandX title =
    Axis.customBandX title


{-| -}
customBandY : Maybe String -> Axis.Axis Horizontal anyDomain
customBandY title =
    Axis.customBandY title


{-| -}
linearX : Maybe String -> Axis Vertical Float
linearX title =
    Axis.linearX title


{-| -}
linearY : Maybe String -> Axis Horizontal Float
linearY title =
    Axis.linearY title


{-| -}
log10X : Maybe String -> Axis Vertical Float
log10X title =
    Axis.log10X title


{-| -}
log10Y : Maybe String -> Axis Horizontal Float
log10Y title =
    Axis.log10Y title


{-| -}
sqrtX : Maybe String -> Axis Vertical Float
sqrtX title =
    Axis.sqrtX title


{-| -}
sqrtY : Maybe String -> Axis Horizontal Float
sqrtY title =
    Axis.sqrtY title



-- modifiers -------------------------------------------------------------------


{-| -}
continuousScale :
    (( domain, domain ) -> ( Float, Float ) -> Scale domain Float)
    -> Axis orientation domain
    -> Axis orientation domain
continuousScale scale axis =
    Axis.continuousScale scale axis


{-| -}
ordinalScale :
    (List domain -> ( Float, Float ) -> Scale domain Float)
    -> Axis orientation domain
    -> Axis orientation domain
ordinalScale scale axis =
    Axis.ordinalScale scale axis


{-| -}
continuousDomain : ( domain, domain ) -> Axis orientation domain -> Axis orientation domain
continuousDomain domain axis =
    Axis.continuousDomain domain axis


{-| -}
ordinalDomain : List domain -> Axis orientation domain -> Axis orientation domain
ordinalDomain domain axis =
    Axis.ordinalDomain domain axis


{-| -}
labelFormat : (domain -> String) -> Axis orientation domain -> Axis orientation domain
labelFormat format axis =
    Axis.format format axis


{-| -}
labelAngle : Float -> Axis.Axis orientation domain -> Axis.Axis orientation domain
labelAngle angle axis =
    Axis.labelAngle angle axis


{-| -}
ticks : Int -> Axis orientation domain -> Axis orientation domain
ticks numTicks axis =
    Axis.ticks numTicks axis


{-| -}
orientLeft : Axis Horizontal domain -> Axis Horizontal domain
orientLeft axis =
    Axis.orientLeft axis


{-| -}
orientRight : Axis Horizontal domain -> Axis Horizontal domain
orientRight axis =
    Axis.orientRight axis


{-| -}
orientTop : Axis Vertical domain -> Axis Vertical domain
orientTop axis =
    Axis.orientTop axis


{-| -}
orientBottom : Axis Vertical domain -> Axis Vertical domain
orientBottom axis =
    Axis.orientBottom axis
