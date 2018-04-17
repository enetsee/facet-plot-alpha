module Facet.Encoding
    exposing
        ( Encoding
        , arc
        , innerRadius
        , padAngle
        , vArea
        , hArea
        , line
        , polygon
        , rect
        , bar
        , cornerRadius
        , rule
        , symbol
        , point
        , arrow
        , cross
        , square
        , diamond
        , trangleUp
        , triangleDown
        , triangleLeft
        , triangleRight
        , shape
        , text
        , relativePosition
        , trail
        , angle
        , fill
        , fillConstant
        , fillOpacity
        , fillOpacityConstant
        , stroke
        , strokeConstant
        , strokeOpacity
        , strokeOpacityConstant
        , strokeWidth
        , strokeWidthConstant
        , strokeDash
        , strokeDashConstant
        , size
        , sizeConstant
        , tooltip
        )

{-|

# Encoding
@docs Encoding

## Arc
@docs arc

@docs innerRadius, padAngle

## Area
@docs vArea, hArea

## Line
@docs line

## Polygon
@docs polygon

## Rect
@docs rect, bar
@docs cornerRadius

## Rule
@docs rule

## Symbol
@docs symbol , point, arrow, cross, square, diamond, trangleUp, triangleDown, triangleLeft, triangleRight , shape

@docs angle

@docs size,sizeConstant

## Text
@docs text
@docs relativePosition

## Trail
@docs trail

## Fill
@docs fill,fillConstant, fillOpacity, fillOpacityConstant


## Stroke
@docs stroke, strokeConstant,  strokeOpacity,strokeOpacityConstant, strokeWidth, strokeWidthConstant, strokeDash,  strokeDashConstant

@docs tooltip

-}

import Color exposing (Color)
import Facet.Internal.Encoding as Encoding
import Facet.Channel as Channel exposing (ShapeChannel, PositionalChannel, ColorChannel, FloatChannel, TextChannel, StrokeDashChannel)
import Facet.Scenegraph.Interpolate exposing (Interpolate)
import Facet.Scenegraph.Mark exposing (Behaviour)
import Facet.Scenegraph.Stroke exposing (StrokeDash)
import Path.LowLevel exposing (SubPath)


{-|
    # Encoding

    An `Encoding` is a means of encoding data as visual mark by combining
    several `Channel`s to represent variaus attributes of that visual mark.

    A description of each encoding along with the required and optional `Channels`
    is given below.

    ## Arc

    A cicular arc.

    ### Required channels

    - x position (`PositionalChannel`)
    - y position (`PositionalChannel`)
    - start angle in Radians (`FloatChannel`)
    - end angle in Radians (`FloatChannel`)
    - outer radius in user-space pixels (`FloatChannel`)

    ### Optional channels

    - inner radius in user-space pixels (`FloatChannel`)
    - corner radius in user-space pixels (`FloatChannel`)
    - fill color (`ColorChannel`)
    - fill opacity, between 0 and 1 (`FloatChannel`)
    - stroke color (`ColorChannel`)
    - stroke opacity, between 0 and 1 (`FloatChannel`)
    - stroke width in user-space pixels (`FloatChannel`)
    - stroke dash (`StrokeDashChannel`)
    - tooltip (`TextChannel`)

    ## Area

    Filled area with either vertical or horizontal orientation.

    ### Required channels

    - x positions (`PositionalChannel`)
    - y positions (`PositionalChannel`)

    You must also provide an interpolation method and the preferred behaviour
    when missing values are encountered

    ### Optional channels

    - fill color (`ColorChannel`)
    - fill opacity, between 0 and 1 (`FloatChannel`)
    - stroke color (`ColorChannel`)
    - stroke opacity, between 0 and 1 (`FloatChannel`)
    - stroke width in user-space pixels (`FloatChannel`)
    - stroke dash (`StrokeDashChannel`)
    - tooltip (`TextChannel`)

    ## Line

    Stroked lines.

    ### Required channels

    - x positions (`PositionalChannel`)
    - y positions (`PositionalChannel`)

    You must also provide an interpolation method and the preferred behaviour
    when missing values are encountered

    ### Optional channels

    - stroke color (`ColorChannel`)
    - stroke opacity, between 0 and 1 (`FloatChannel`)
    - stroke width in user-space pixels (`FloatChannel`)
    - stroke dash (`StrokeDashChannel`)
    - tooltip (`TextChannel`)

    ## Polygon

    Arbitrary filled polygons.

    ### Required channels

    - x positions (`PositionalChannel`)
    - y positions (`PositionalChannel`)

    You must also provide an interpolation method and the preferred behaviour
    when missing values are encountered

    ### Optional channels

    - fill color (`ColorChannel`)
    - fill opacity, between 0 and 1 (`FloatChannel`)
    - stroke color (`ColorChannel`)
    - stroke opacity, between 0 and 1 (`FloatChannel`)
    - stroke width in user-space pixels (`FloatChannel`)
    - stroke dash (`StrokeDashChannel`)
    - tooltip (`TextChannel`)

    ## Rect

    Filled rectangles.

    ### Required channels

    Either
    - primary and secondary x and y positions
    or
    - primary x and y positions, width and height

    ### Optional channels

    - corner radius in user-space pixels (`FloatChannel`)
    - fill color (`ColorChannel`)
    - fill opacity, between 0 and 1 (`FloatChannel`)
    - stroke color (`ColorChannel`)
    - stroke opacity, between 0 and 1 (`FloatChannel`)
    - stroke width in user-space pixels (`FloatChannel`)
    - stroke dash (`StrokeDashChannel`)
    - tooltip (`TextChannel`)

    ## Rule

    Strkoed line segments.

    ### Required channels

    - primary and secondary x positions (`PositionalChannel`)
    - primary and secondary y positions (`PositionalChannel`)

    ### Optional channels

    - stroke color (`ColorChannel`)
    - stroke opacity, between 0 and 1 (`FloatChannel`)
    - stroke width in user-space pixels (`FloatChannel`)
    - stroke dash (`StrokeDashChannel`)
    - tooltip (`TextChannel`)

    ## Symbol

    Plotting symbols, including circles, squares and other shapes.

    ### Required channels

    - shape (`ShapeChannel`)
    - x position (`PositionalChannel`)
    - y position (`PositionalChannel`)

    ### Optional channels

    - size in user-space pixels squared (`FloatChannel`)
    - angle in Radians (`FloatChannel`)
    - fill color (`ColorChannel`)
    - fill opacity, between 0 and 1 (`FloatChannel`)
    - stroke color (`ColorChannel`)
    - stroke opacity, between 0 and 1 (`FloatChannel`)
    - stroke width in user-space pixels (`FloatChannel`)
    - stroke dash (`StrokeDashChannel`)
    - tooltip (`TextChannel`)

    ## Text

    Text labels with configurable fonts, alignment and angle.

    ### Required Channels

    - text (`TextChannel`)
    - x position (`PositionalChannel`)
    - y position (`PositionalChannel`)


    ### Optional channels

    - size in user-space pixels squared (`FloatChannel`)
    - angle in Radians (`FloatChannel`)
    - fill color (`ColorChannel`)
    - fill opacity, between 0 and 1 (`FloatChannel`)
    - stroke color (`ColorChannel`)
    - stroke opacity, between 0 and 1 (`FloatChannel`)
    - stroke width in user-space pixels (`FloatChannel`)
    - stroke dash (`StrokeDashChannel`)
    - tooltip (`TextChannel`)

    ## Trail

    Filled lines with varying width.

    ### Required Channels

    - widths (`FloatChannel`)
    - x positions (`PositionalChannel`)
    - y positions (`PositionalChannel`)


    ### Optional channels

    - fill color (`ColorChannel`)
    - fill opacity, between 0 and 1 (`FloatChannel`)
    - tooltip (`TextChannel`)

-}
type alias Encoding data xdomain ydomain =
    Encoding.Encoding data xdomain ydomain



-- Arc -------------------------------------------------------------------------


{-| Circular arc.
-}
arc :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> FloatChannel data
    -> FloatChannel data
    -> FloatChannel data
    -> Encoding data xdomain ydomain
arc x y startAngle endAngle outerRadius =
    Encoding.arc x y startAngle endAngle outerRadius


{-| -}
innerRadius :
    FloatChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
innerRadius channel encoding =
    Encoding.innerRadius channel encoding


{-| -}
padAngle :
    FloatChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
padAngle channel encoding =
    Encoding.padAngle channel encoding



-- Area ------------------------------------------------------------------------


{-| -}
vArea :
    PositionalChannel data xdomain
    -> PositionalChannel data number
    -> Interpolate
    -> Behaviour
    -> Encoding data xdomain number
vArea xs ys interpolate behaviour =
    Encoding.vArea xs ys interpolate behaviour


{-| -}
hArea :
    PositionalChannel data xdomain
    -> PositionalChannel data number
    -> Interpolate
    -> Behaviour
    -> Encoding data xdomain number
hArea xs ys interpolate behaviour =
    Encoding.hArea xs ys interpolate behaviour



-- Line ------------------------------------------------------------------------


{-| -}
line :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Interpolate
    -> Behaviour
    -> Encoding data xdomain ydomain
line xs ys interpolate behaviour =
    Encoding.line xs ys interpolate behaviour



-- Polygon ---------------------------------------------------------------------


{-| -}
polygon :
    PositionalChannel data xdomain
    -> PositionalChannel data number
    -> Interpolate
    -> Behaviour
    -> Encoding data xdomain number
polygon xs ys interpolate behaviour =
    Encoding.polygon xs ys interpolate behaviour



-- Rect ------------------------------------------------------------------------


{-| -}
rect :
    PositionalChannel data xdomain
    -> PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
rect x x2 y y2 =
    Encoding.rect x x2 y y2


{-| -}
bar :
    PositionalChannel data xdomain
    -> Float
    -> PositionalChannel data Float
    -> Encoding data xdomain Float
bar x width height =
    Encoding.bar x (sqrt width) height



-- Rule ------------------------------------------------------------------------


{-| -}
rule :
    PositionalChannel data xdomain
    -> PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
rule x x2 y y2 =
    Encoding.rule x x2 y y2



-- Symbol ----------------------------------------------------------------------


{-| -}
symbol :
    ShapeChannel data
    -> PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
symbol shape x y =
    Encoding.symbol shape x y


{-| -}
point :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
point x y =
    Encoding.point x y


{-| -}
arrow :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
arrow x y =
    Encoding.arrow x y


{-| -}
cross :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
cross x y =
    Encoding.cross x y


{-| -}
square :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
square x y =
    Encoding.square x y


{-| -}
diamond :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
diamond x y =
    Encoding.diamond x y


{-| -}
trangleUp :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
trangleUp x y =
    Encoding.trangleUp x y


{-| -}
triangleDown :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
triangleDown x y =
    Encoding.triangleDown x y


{-| -}
triangleLeft :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
triangleLeft x y =
    Encoding.triangleLeft x y


{-| -}
triangleRight :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
triangleRight x y =
    Encoding.triangleRight x y


{-| -}
shape :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> SubPath
    -> Encoding data xdomain ydomain
shape x y subpath =
    Encoding.shape x y subpath



-- Text ------------------------------------------------------------------------


{-| -}
text :
    TextChannel data
    -> PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
text text x y =
    Encoding.text text x y


{-| -}
relativePosition :
    Float
    -> Float
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
relativePosition dx dy encoding =
    Encoding.relativePosition dx dy encoding



-- Trail ------------------------------------------------------------------------


{-| -}
trail :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> FloatChannel data
    -> Behaviour
    -> Encoding data xdomain ydomain
trail xs ys widths behaviour =
    Encoding.trail xs ys widths behaviour



-- General ---------------------------------------------------------------------


{-| -}
size :
    FloatChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
size channel encoding =
    Encoding.size channel encoding


{-| -}
sizeConstant :
    Float
    -> Encoding.Encoding data xdomain ydomain
    -> Encoding.Encoding data xdomain ydomain
sizeConstant size encoding =
    Encoding.sizeConstant size encoding


{-| -}
angle :
    FloatChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
angle channel encoding =
    Encoding.angle channel encoding


{-| -}
cornerRadius :
    FloatChannel data
    -> Encoding.Encoding data xdomain ydomain
    -> Encoding.Encoding data xdomain ydomain
cornerRadius channel encoding =
    Encoding.cornerRadius channel encoding


{-| -}
tooltip :
    TextChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
tooltip channel encoding =
    Encoding.tooltip channel encoding



-- Fill ------------------------------------------------------------------------


{-| -}
fill :
    ColorChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
fill fill encoding =
    Encoding.fill fill encoding


{-| -}
fillConstant :
    Color
    -> Encoding.Encoding data xdomain ydomain
    -> Encoding.Encoding data xdomain ydomain
fillConstant color encoding =
    Encoding.fillConstant color encoding


{-| -}
fillOpacity :
    FloatChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
fillOpacity fillOpacity encoding =
    Encoding.fillOpacity fillOpacity encoding


{-| -}
fillOpacityConstant :
    Float
    -> Encoding.Encoding data xdomain ydomain
    -> Encoding.Encoding data xdomain ydomain
fillOpacityConstant alpha encoding =
    Encoding.fillOpacityConstant alpha encoding



-- Stroke ----------------------------------------------------------------------


{-| -}
stroke :
    ColorChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
stroke channel encoding =
    Encoding.stroke channel encoding


{-| -}
strokeConstant :
    Color
    -> Encoding.Encoding data xdomain ydomain
    -> Encoding.Encoding data xdomain ydomain
strokeConstant color encoding =
    Encoding.strokeConstant color encoding


{-| -}
strokeOpacity :
    FloatChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
strokeOpacity channel encoding =
    Encoding.strokeOpacity channel encoding


{-| -}
strokeOpacityConstant :
    Float
    -> Encoding.Encoding data xdomain ydomain
    -> Encoding.Encoding data xdomain ydomain
strokeOpacityConstant alpha encoding =
    Encoding.strokeOpacityConstant alpha encoding


{-| -}
strokeWidth :
    FloatChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
strokeWidth channel encoding =
    Encoding.strokeWidth channel encoding


{-| -}
strokeWidthConstant :
    Float
    -> Encoding.Encoding data xdomain ydomain
    -> Encoding.Encoding data xdomain ydomain
strokeWidthConstant width encoding =
    Encoding.strokeWidthConstant width encoding


{-| -}
strokeDash :
    StrokeDashChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
strokeDash channel encoding =
    Encoding.strokeDash channel encoding


{-| -}
strokeDashConstant :
    StrokeDash
    -> Encoding.Encoding data xdomain ydomain
    -> Encoding.Encoding data xdomain ydomain
strokeDashConstant dash encoding =
    Encoding.strokeDashConstant dash encoding
