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
    { a
        | x : PositionalChannel data xdomain
        , y : PositionalChannel data ydomain
        , startAngle : FloatChannel data
        , endAngle : FloatChannel data
        , outerRadius : FloatChannel data
    }
    -> Encoding data xdomain ydomain
arc { x, y, startAngle, endAngle, outerRadius } =
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
    { a
        | x : PositionalChannel data xdomain
        , y : PositionalChannel data number
        , interpolate : Interpolate
        , onMissing : Behaviour
    }
    -> Encoding data xdomain number
vArea { x, y, interpolate, onMissing } =
    Encoding.vArea x y interpolate onMissing


{-| -}
hArea :
    { a
        | x : PositionalChannel data xdomain
        , y : PositionalChannel data number
        , interpolate : Interpolate
        , onMissing : Behaviour
    }
    -> Encoding data xdomain number
hArea { x, y, interpolate, onMissing } =
    Encoding.hArea x y interpolate onMissing



-- Line ------------------------------------------------------------------------


{-| -}
line :
    { a
        | x : PositionalChannel data xdomain
        , y : PositionalChannel data ydomain
        , interpolate : Interpolate
        , onMissing : Behaviour
    }
    -> Encoding data xdomain ydomain
line { x, y, interpolate, onMissing } =
    Encoding.line x y interpolate onMissing



-- Polygon ---------------------------------------------------------------------


{-| -}
polygon :
    { a
        | x : PositionalChannel data xdomain
        , y : PositionalChannel data number
        , interpolate : Interpolate
        , onMissing : Behaviour
    }
    -> Encoding data xdomain number
polygon { x, y, interpolate, onMissing } =
    Encoding.polygon x y interpolate onMissing



-- Rect ------------------------------------------------------------------------


{-| -}
rect :
    { a
        | x : PositionalChannel data xdomain
        , x2 : PositionalChannel data xdomain
        , y : PositionalChannel data ydomain
        , y2 : PositionalChannel data ydomain
    }
    -> Encoding data xdomain ydomain
rect { x, x2, y, y2 } =
    Encoding.rect x x2 y y2


{-| -}
bar :
    { a
        | x : PositionalChannel data xdomain
        , width : Float
        , height : PositionalChannel data Float
    }
    -> Encoding data xdomain Float
bar { x, width, height } =
    Encoding.bar x (sqrt width) height



-- Rule ------------------------------------------------------------------------


{-| -}
rule :
    { a
        | x : PositionalChannel data xdomain
        , x2 : PositionalChannel data xdomain
        , y : PositionalChannel data ydomain
        , y2 : PositionalChannel data ydomain
    }
    -> Encoding data xdomain ydomain
rule { x, x2, y, y2 } =
    Encoding.rule x x2 y y2



-- Symbol ----------------------------------------------------------------------


{-| -}
symbol :
    { a
        | shape : ShapeChannel data
        , x : PositionalChannel data xdomain
        , y : PositionalChannel data ydomain
    }
    -> Encoding data xdomain ydomain
symbol { shape, x, y } =
    Encoding.symbol shape x y


{-| -}
point :
    { a
        | x : PositionalChannel data xdomain
        , y : PositionalChannel data ydomain
    }
    -> Encoding data xdomain ydomain
point { x, y } =
    Encoding.point x y


{-| -}
arrow :
    { a
        | x : PositionalChannel data xdomain
        , y : PositionalChannel data ydomain
    }
    -> Encoding data xdomain ydomain
arrow { x, y } =
    Encoding.arrow x y


{-| -}
cross :
    { a
        | x : PositionalChannel data xdomain
        , y : PositionalChannel data ydomain
    }
    -> Encoding data xdomain ydomain
cross { x, y } =
    Encoding.cross x y


{-| -}
square :
    { a
        | x : PositionalChannel data xdomain
        , y : PositionalChannel data ydomain
    }
    -> Encoding data xdomain ydomain
square { x, y } =
    Encoding.square x y


{-| -}
diamond :
    { a
        | x : PositionalChannel data xdomain
        , y : PositionalChannel data ydomain
    }
    -> Encoding data xdomain ydomain
diamond { x, y } =
    Encoding.diamond x y


{-| -}
trangleUp :
    { a
        | x : PositionalChannel data xdomain
        , y : PositionalChannel data ydomain
    }
    -> Encoding data xdomain ydomain
trangleUp { x, y } =
    Encoding.trangleUp x y


{-| -}
triangleDown :
    { a
        | x : PositionalChannel data xdomain
        , y : PositionalChannel data ydomain
    }
    -> Encoding data xdomain ydomain
triangleDown { x, y } =
    Encoding.triangleDown x y


{-| -}
triangleLeft :
    { a
        | x : PositionalChannel data xdomain
        , y : PositionalChannel data ydomain
    }
    -> Encoding data xdomain ydomain
triangleLeft { x, y } =
    Encoding.triangleLeft x y


{-| -}
triangleRight :
    { a
        | x : PositionalChannel data xdomain
        , y : PositionalChannel data ydomain
    }
    -> Encoding data xdomain ydomain
triangleRight { x, y } =
    Encoding.triangleRight x y


{-| -}
shape :
    { a
        | x : PositionalChannel data xdomain
        , y : PositionalChannel data ydomain
        , path : SubPath
    }
    -> Encoding data xdomain ydomain
shape { x, y, path } =
    Encoding.shape x y path



-- Text ------------------------------------------------------------------------


{-| -}
text :
    { a
        | text : TextChannel data
        , x : PositionalChannel data xdomain
        , y : PositionalChannel data ydomain
    }
    -> Encoding data xdomain ydomain
text { text, x, y } =
    Encoding.text text x y


{-| -}
relativePosition :
    { dx : Float
    , dy : Float
    }
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
relativePosition { dx, dy } encoding =
    Encoding.relativePosition dx dy encoding



-- Trail ------------------------------------------------------------------------


{-| -}
trail :
    { a
        | x : PositionalChannel data xdomain
        , y : PositionalChannel data ydomain
        , width : FloatChannel data
        , onMissing : Behaviour
    }
    -> Encoding data xdomain ydomain
trail { x, y, width, onMissing } =
    Encoding.trail x y width onMissing



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
