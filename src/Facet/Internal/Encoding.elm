module Facet.Internal.Encoding
    exposing
        ( Encoding
        , scenegraph
        , legends
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
        , hasConstantXPosition
        , hasConstantYPosition
        , extentXContinuous
        , extentXDiscrete
        , extentYContinuous
        , extentYDiscrete
        )

import Color exposing (Color)
import Facet.Internal.Channel as Channel exposing (ShapeChannel, PositionalChannel, ColorChannel, FloatChannel, TextChannel, StrokeDashChannel)
import Facet.Internal.Encoding.Fill as Fill exposing (Fill)
import Facet.Internal.Encoding.Stroke as Stroke exposing (Stroke)
import Facet.Internal.Encoding.Arc as Arc
import Facet.Internal.Encoding.Area as Area
import Facet.Internal.Encoding.Line as Line
import Facet.Internal.Encoding.Polygon as Polygon
import Facet.Internal.Encoding.Rect as Rect
import Facet.Internal.Encoding.Rule as Rule
import Facet.Internal.Encoding.Symbol as Symbol
import Facet.Internal.Encoding.Text as Text
import Facet.Internal.Encoding.Trail as Trail
import Facet.Internal.Encoding.Position as Position
import Facet.Internal.Field as Field
import Facet.Internal.Legend exposing (LegendSpec)
import Facet.Helpers exposing (minWith, maxWith)
import Facet.List.Extra as List
import Facet.Maybe.Extra as Maybe
import Facet.Scale as Scale exposing (Scale)
import Facet.Scenegraph exposing (Scenegraph)
import Facet.Scenegraph.Interpolate exposing (Interpolate)
import Facet.Scenegraph.Mark exposing (Behaviour)
import Facet.Scenegraph.Stroke exposing (StrokeDash)
import Facet.Theme as Theme
import Path.LowLevel exposing (SubPath)


type Encoding data xdomain ydomain
    = Arc (Arc.Arc data xdomain ydomain)
    | Area (Area.Area data xdomain ydomain)
    | Line (Line.Line data xdomain ydomain)
    | Polygon (Polygon.Polygon data xdomain ydomain)
    | Rect (Rect.Rect data xdomain ydomain)
    | Rule (Rule.Rule data xdomain ydomain)
    | Symbol (Symbol.Symbol data xdomain ydomain)
    | Text (Text.Text data xdomain ydomain)
    | Trail (Trail.Trail data xdomain ydomain)


scenegraph :
    Theme.Mark
    -> Scale xdomain Float
    -> Scale ydomain Float
    -> List data
    -> Encoding data xdomain ydomain
    -> Scenegraph
scenegraph theme xScale yScale data encoding =
    case encoding of
        Arc encoding ->
            Arc.scenegraph theme.arc xScale yScale data encoding

        Area encoding ->
            Area.scenegraph theme.area xScale yScale data encoding

        Line encoding ->
            Line.scenegraph theme.line xScale yScale data encoding

        Polygon encoding ->
            Polygon.scenegraph theme.polygon xScale yScale data encoding

        Rect encoding ->
            Rect.scenegraph theme.rect xScale yScale data encoding

        Rule encoding ->
            Rule.scenegraph theme.rule xScale yScale data encoding

        Symbol encoding ->
            Symbol.scenegraph theme.symbol xScale yScale data encoding

        Text encoding ->
            Text.scenegraph theme.text xScale yScale data encoding

        Trail encoding ->
            Trail.scenegraph theme.trail xScale yScale data encoding



-- legend List data -> Encoding data xdomain ydomain -> ?


legends : Int -> Encoding data xdomain ydomain -> LegendSpec
legends ticks encoding =
    case encoding of
        Area encoding ->
            Area.legends ticks encoding

        Arc encoding ->
            Arc.legends ticks encoding

        Line encoding ->
            Line.legends ticks encoding

        Polygon encoding ->
            Polygon.legends ticks encoding

        Rect encoding ->
            Rect.legends ticks encoding

        Rule encoding ->
            Rule.legends ticks encoding

        Symbol encoding ->
            Symbol.legends ticks encoding

        Text encoding ->
            Text.legends ticks encoding

        Trail encoding ->
            Trail.legends ticks encoding



-- Constructors ----------------------------------------------------------------
-- Arc -------------------------------------------------------------------------


arc :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> FloatChannel data
    -> FloatChannel data
    -> FloatChannel data
    -> Encoding data xdomain ydomain
arc x y startAngle endAngle outerRadius =
    Arc <| Arc.arc x y startAngle endAngle outerRadius


innerRadius :
    FloatChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
innerRadius channel encoding =
    case encoding of
        Arc e ->
            Arc { e | innerRadius = Just channel }

        _ ->
            encoding


padAngle :
    FloatChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
padAngle channel encoding =
    case encoding of
        Arc e ->
            Arc { e | padAngle = Just channel }

        _ ->
            encoding



-- Area ------------------------------------------------------------------------


hArea :
    PositionalChannel data xdomain
    -> PositionalChannel data number
    -> Interpolate
    -> Behaviour
    -> Encoding data xdomain number
hArea xs ys interpolate behaviour =
    Area <| Area.hArea xs ys interpolate behaviour


vArea :
    PositionalChannel data xdomain
    -> PositionalChannel data number
    -> Interpolate
    -> Behaviour
    -> Encoding data xdomain number
vArea xs ys interpolate behaviour =
    Area <| Area.vArea xs ys interpolate behaviour



-- Line ------------------------------------------------------------------------


line :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Interpolate
    -> Behaviour
    -> Encoding data xdomain ydomain
line xs ys interpolate behaviour =
    Line <| Line.line xs ys interpolate behaviour



-- Polygon ---------------------------------------------------------------------


polygon :
    PositionalChannel data xdomain
    -> PositionalChannel data number
    -> Interpolate
    -> Behaviour
    -> Encoding data xdomain number
polygon xs ys interpolate behaviour =
    Polygon <| Polygon.polygon xs ys interpolate behaviour



-- Rect ------------------------------------------------------------------------


rect :
    PositionalChannel data xdomain
    -> PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
rect x x2 y y2 =
    Rect <| Rect.rect x x2 y y2


bar :
    PositionalChannel data xdomain
    -> Float
    -> PositionalChannel data Float
    -> Encoding data xdomain Float
bar x width height =
    let
        widthChannel =
            Field.constant width
                |> Channel.float toString (Scale.constant width)

        yChannel =
            { height | field = Field.constant 0 }
    in
        Rect.bar x widthChannel yChannel height
            |> Rect


cornerRadius :
    FloatChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
cornerRadius channel encoding =
    case encoding of
        Rect e ->
            Rect { e | cornerRadius = Just channel }

        Arc e ->
            Arc { e | cornerRadius = Just channel }

        _ ->
            encoding



-- Rule ------------------------------------------------------------------------


rule :
    PositionalChannel data xdomain
    -> PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
rule x x2 y y2 =
    Rule <| Rule.rule x x2 y y2



-- Symbol ------------------------------------------------------------------------


symbol :
    ShapeChannel data
    -> PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
symbol shape x y =
    Symbol <| Symbol.symbol shape x y


point :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
point x y =
    Symbol <| Symbol.point x y


arrow :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
arrow x y =
    Symbol <| Symbol.arrow x y


cross :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
cross x y =
    Symbol <| Symbol.cross x y


square :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
square x y =
    Symbol <| Symbol.square x y


diamond :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
diamond x y =
    Symbol <| Symbol.diamond x y


trangleUp :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
trangleUp x y =
    Symbol <| Symbol.triangleUp x y


triangleDown :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
triangleDown x y =
    Symbol <| Symbol.triangleDown x y


triangleLeft :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
triangleLeft x y =
    Symbol <| Symbol.triangleLeft x y


triangleRight :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
triangleRight x y =
    Symbol <| Symbol.triangleRight x y


shape :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> SubPath
    -> Encoding data xdomain ydomain
shape x y subpath =
    Symbol <| Symbol.shape x y subpath



-- Text ------------------------------------------------------------------------


text :
    TextChannel data
    -> PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> Encoding data xdomain ydomain
text text x y =
    Text <| Text.text text x y


relativePosition :
    Float
    -> Float
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
relativePosition dx dy encoding =
    case encoding of
        Text e ->
            Text { e | dx = Just dx, dy = Just dy }

        _ ->
            encoding



-- Trail -----------------------------------------------------------------------


trail :
    PositionalChannel data xdomain
    -> PositionalChannel data ydomain
    -> FloatChannel data
    -> Behaviour
    -> Encoding data xdomain ydomain
trail xs ys widths behaviour =
    Trail <| Trail.trail xs ys widths behaviour



-- Size ------------------------------------------------------------------------


size :
    FloatChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
size channel encoding =
    case encoding of
        Symbol encoding ->
            Symbol <| { encoding | size = Just channel }

        Text encoding ->
            Text <| { encoding | size = Just channel }

        {- No size channel -}
        Area _ ->
            encoding

        Line _ ->
            encoding

        Rect _ ->
            encoding

        Rule _ ->
            encoding

        Trail _ ->
            encoding

        Arc _ ->
            encoding

        Polygon _ ->
            encoding


sizeConstant :
    Float
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
sizeConstant size_ encoding =
    let
        channel =
            Channel.float toString (Scale.constant size_) (Field.constant 0)
    in
        size channel encoding



-- Tooltip ---------------------------------------------------------------------
--


tooltip :
    Channel.TextChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
tooltip channel encoding =
    case encoding of
        Arc encoding ->
            Arc <| { encoding | tooltip = Just channel }

        Area encoding ->
            Area <| { encoding | tooltip = Just channel }

        Line encoding ->
            Line <| { encoding | tooltip = Just channel }

        Polygon encoding ->
            Polygon <| { encoding | tooltip = Just channel }

        Rect encoding ->
            Rect <| { encoding | tooltip = Just channel }

        Rule encoding ->
            Rule <| { encoding | tooltip = Just channel }

        Symbol encoding ->
            Symbol <| { encoding | tooltip = Just channel }

        Text encoding ->
            Text <| { encoding | tooltip = Just channel }

        Trail encoding ->
            Trail <| { encoding | tooltip = Just channel }



-- Angle -----------------------------------------------------------------------


angle :
    FloatChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
angle channel encoding =
    case encoding of
        Symbol encoding ->
            Symbol <| { encoding | angle = Just channel }

        Text encoding ->
            Text <| { encoding | angle = Just channel }

        {- No angle channel -}
        Area _ ->
            encoding

        Line _ ->
            encoding

        Rect _ ->
            encoding

        Rule _ ->
            encoding

        Trail _ ->
            encoding

        Arc _ ->
            encoding

        Polygon _ ->
            encoding



-- Fill ------------------------------------------------------------------------


fill :
    ColorChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
fill fill encoding =
    case encoding of
        Arc encoding ->
            Arc <| withFillHelper fill encoding

        Area encoding ->
            Area <| withFillHelper fill encoding

        Line _ ->
            encoding

        Polygon encoding ->
            Polygon <| withFillHelper fill encoding

        Rect encoding ->
            Rect <| withFillHelper fill encoding

        Rule _ ->
            encoding

        Symbol encoding ->
            Symbol <| withFillHelper fill encoding

        Text encoding ->
            Text <| withFillHelper fill encoding

        Trail encoding ->
            Trail <| withFillHelper fill encoding


fillConstant :
    Color
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
fillConstant color encoding =
    let
        channel =
            Channel.color toString (Scale.constant color) (Field.constant 0)
    in
        fill channel encoding


withFillHelper :
    ColorChannel data
    -> { a | fill : Maybe (Fill data) }
    -> { a | fill : Maybe (Fill data) }
withFillHelper fill encoding =
    let
        oldFill =
            Maybe.withDefault Fill.empty encoding.fill

        newFill =
            { oldFill | fill = Just fill }
    in
        { encoding | fill = Just newFill }


fillOpacity :
    FloatChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
fillOpacity fillOpacity encoding =
    case encoding of
        Arc encoding ->
            Arc <| withFillOpacityHelper fillOpacity encoding

        Area encoding ->
            Area <| withFillOpacityHelper fillOpacity encoding

        Line _ ->
            encoding

        Polygon encoding ->
            Polygon <| withFillOpacityHelper fillOpacity encoding

        Rect encoding ->
            Rect <| withFillOpacityHelper fillOpacity encoding

        Rule _ ->
            encoding

        Symbol encoding ->
            Symbol <| withFillOpacityHelper fillOpacity encoding

        Text encoding ->
            Text <| withFillOpacityHelper fillOpacity encoding

        Trail encoding ->
            Trail <| withFillOpacityHelper fillOpacity encoding


withFillOpacityHelper :
    FloatChannel data
    -> { a | fill : Maybe (Fill data) }
    -> { a | fill : Maybe (Fill data) }
withFillOpacityHelper fillOpacity encoding =
    let
        oldFill =
            Maybe.withDefault Fill.empty encoding.fill

        newFill =
            { oldFill | fillOpacity = Just fillOpacity }
    in
        { encoding | fill = Just newFill }


fillOpacityConstant :
    Float
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
fillOpacityConstant alpha encoding =
    let
        channel =
            Channel.float toString (Scale.constant alpha) (Field.constant alpha)
    in
        fillOpacity channel encoding



-- Stroke ------------------------------------------------------------------------


stroke :
    ColorChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
stroke channel encoding =
    case encoding of
        Arc encoding ->
            Arc <| withStrokeHelper channel encoding

        Area encoding ->
            Area <| withStrokeHelper channel encoding

        Line encoding ->
            Line <| withStrokeHelper channel encoding

        Polygon encoding ->
            Polygon <| withStrokeHelper channel encoding

        Rect encoding ->
            Rect <| withStrokeHelper channel encoding

        Rule encoding ->
            Rule <| withStrokeHelper channel encoding

        Symbol encoding ->
            Symbol <| withStrokeHelper channel encoding

        Text encoding ->
            Text <| withStrokeHelper channel encoding

        Trail _ ->
            encoding


withStrokeHelper :
    ColorChannel data
    -> { a | stroke : Maybe (Stroke data) }
    -> { a | stroke : Maybe (Stroke data) }
withStrokeHelper channel encoding =
    let
        old =
            Maybe.withDefault Stroke.empty encoding.stroke

        new =
            { old | stroke = Just channel }
    in
        { encoding | stroke = Just new }


strokeConstant :
    Color
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
strokeConstant color encoding =
    let
        channel =
            Channel.color toString (Scale.constant color) (Field.constant 0)
    in
        stroke channel encoding


strokeOpacity :
    FloatChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
strokeOpacity channel encoding =
    case encoding of
        Arc encoding ->
            Arc <| withStrokeOpacityHelper channel encoding

        Area encoding ->
            Area <| withStrokeOpacityHelper channel encoding

        Line encoding ->
            Line <| withStrokeOpacityHelper channel encoding

        Rect encoding ->
            Rect <| withStrokeOpacityHelper channel encoding

        Polygon encoding ->
            Polygon <| withStrokeOpacityHelper channel encoding

        Rule encoding ->
            Rule <| withStrokeOpacityHelper channel encoding

        Symbol encoding ->
            Symbol <| withStrokeOpacityHelper channel encoding

        Text encoding ->
            Text <| withStrokeOpacityHelper channel encoding

        Trail _ ->
            encoding


withStrokeOpacityHelper :
    FloatChannel data
    -> { a | stroke : Maybe (Stroke data) }
    -> { a | stroke : Maybe (Stroke data) }
withStrokeOpacityHelper channel encoding =
    let
        old =
            Maybe.withDefault Stroke.empty encoding.stroke

        new =
            { old | strokeOpacity = Just channel }
    in
        { encoding | stroke = Just new }


strokeOpacityConstant :
    Float
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
strokeOpacityConstant alpha encoding =
    let
        channel =
            Channel.float toString (Scale.constant alpha) (Field.constant 0)
    in
        strokeOpacity channel encoding


strokeWidth :
    FloatChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
strokeWidth channel encoding =
    case encoding of
        Arc encoding ->
            Arc <| withStrokeWidthHelper channel encoding

        Area encoding ->
            Area <| withStrokeWidthHelper channel encoding

        Line encoding ->
            Line <| withStrokeWidthHelper channel encoding

        Polygon encoding ->
            Polygon <| withStrokeWidthHelper channel encoding

        Rect encoding ->
            Rect <| withStrokeWidthHelper channel encoding

        Rule encoding ->
            Rule <| withStrokeWidthHelper channel encoding

        Symbol encoding ->
            Symbol <| withStrokeWidthHelper channel encoding

        Text encoding ->
            Text <| withStrokeWidthHelper channel encoding

        Trail _ ->
            encoding


withStrokeWidthHelper :
    FloatChannel data
    -> { a | stroke : Maybe (Stroke data) }
    -> { a | stroke : Maybe (Stroke data) }
withStrokeWidthHelper channel encoding =
    let
        old =
            Maybe.withDefault Stroke.empty encoding.stroke

        new =
            { old | strokeWidth = Just channel }
    in
        { encoding | stroke = Just new }


strokeWidthConstant :
    Float
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
strokeWidthConstant width encoding =
    let
        channel =
            Channel.float toString (Scale.constant width) (Field.constant 0)
    in
        strokeWidth channel encoding


strokeDash :
    StrokeDashChannel data
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
strokeDash channel encoding =
    case encoding of
        Arc encoding ->
            Arc <| withStrokeDashHelper channel encoding

        Area encoding ->
            Area <| withStrokeDashHelper channel encoding

        Line encoding ->
            Line <| withStrokeDashHelper channel encoding

        Rect encoding ->
            Rect <| withStrokeDashHelper channel encoding

        Polygon encoding ->
            Polygon <| withStrokeDashHelper channel encoding

        Rule encoding ->
            Rule <| withStrokeDashHelper channel encoding

        Symbol encoding ->
            Symbol <| withStrokeDashHelper channel encoding

        Text encoding ->
            Text <| withStrokeDashHelper channel encoding

        Trail _ ->
            encoding


withStrokeDashHelper :
    StrokeDashChannel data
    -> { a | stroke : Maybe (Stroke data) }
    -> { a | stroke : Maybe (Stroke data) }
withStrokeDashHelper channel encoding =
    let
        old =
            Maybe.withDefault Stroke.empty encoding.stroke

        new =
            { old | strokeDash = Just channel }
    in
        { encoding | stroke = Just new }


strokeDashConstant :
    StrokeDash
    -> Encoding data xdomain ydomain
    -> Encoding data xdomain ydomain
strokeDashConstant dash encoding =
    let
        channel =
            Channel.strokeDash toString (Scale.constant dash) (Field.constant 0)
    in
        strokeDash channel encoding



-- Check properties of encodings for compile errors ----------------------------


hasConstantXPosition : Encoding data xdomain ydomain -> Maybe Bool
hasConstantXPosition encoding =
    case encoding of
        Arc encoding ->
            Just <| Field.isConstant encoding.x.field

        Area encoding ->
            Just <| Field.isConstant encoding.x.field

        Line encoding ->
            Just <| Field.isConstant encoding.x.field

        Polygon encoding ->
            Just <| Field.isConstant encoding.x.field

        Rect encoding ->
            Just <| Position.isConstant encoding.x

        Rule encoding ->
            Just <| Position.isConstant encoding.x

        Symbol encoding ->
            Just <| Field.isConstant encoding.x.field

        Text encoding ->
            Just <| Field.isConstant encoding.x.field

        Trail encoding ->
            Just <| Field.isConstant encoding.x.field


hasConstantYPosition : Encoding data xdomain ydomain -> Maybe Bool
hasConstantYPosition encoding =
    case encoding of
        Arc encoding ->
            Just <| Field.isConstant encoding.y.field

        Area encoding ->
            Just <| Position.isConstant encoding.y

        Line encoding ->
            Just <| Field.isConstant encoding.y.field

        Polygon encoding ->
            Just <| Field.isConstant encoding.x.field

        Rect encoding ->
            Just <| Position.isConstant encoding.y

        Rule encoding ->
            Just <| Position.isConstant encoding.y

        Symbol encoding ->
            Just <| Field.isConstant encoding.y.field

        Text encoding ->
            Just <| Field.isConstant encoding.x.field

        Trail encoding ->
            Just <| Field.isConstant encoding.y.field



-- DOMAIN EXTENTS --------------------------------------------------------------


extentXContinuous :
    List data
    -> Encoding data xdomain ydomain
    -> ( Maybe xdomain, Maybe xdomain )
    -> ( Maybe xdomain, Maybe xdomain )
extentXContinuous data encoding extent =
    extentX extentContinuous data encoding extent


extentXDiscrete : List data -> Encoding data xdomain ydomain -> List xdomain -> List xdomain
extentXDiscrete data encoding extent =
    extentX extentDiscrete data encoding extent


extentYContinuous :
    List data
    -> Encoding data xdomain ydomain
    -> ( Maybe ydomain, Maybe ydomain )
    -> ( Maybe ydomain, Maybe ydomain )
extentYContinuous data encoding extent =
    extentY extentContinuous data encoding extent


extentYDiscrete : List data -> Encoding data xdomain ydomain -> List ydomain -> List ydomain
extentYDiscrete data encoding extent =
    extentY extentDiscrete data encoding extent


extentX :
    (PositionalChannel data xdomain -> a -> b -> b)
    -> a
    -> Encoding data xdomain ydomain
    -> b
    -> b
extentX f data encoding extent =
    case encoding of
        Arc encoding ->
            f encoding.x data extent

        Area encoding ->
            f encoding.x data extent

        Line encoding ->
            f encoding.x data extent

        Polygon encoding ->
            f encoding.x data extent

        Rect encoding ->
            extentWithPosition f encoding.x data extent

        Rule encoding ->
            extentWithPosition f encoding.x data extent

        Symbol encoding ->
            f encoding.x data extent

        Text encoding ->
            f encoding.x data extent

        Trail encoding ->
            f encoding.x data extent


extentY :
    (PositionalChannel data ydomain -> a -> b -> b)
    -> a
    -> Encoding data xdomain ydomain
    -> b
    -> b
extentY f data encoding extent =
    case encoding of
        Arc encoding ->
            f encoding.y data extent

        Area encoding ->
            extentWithPosition f encoding.y data extent

        Line encoding ->
            f encoding.y data extent

        Polygon encoding ->
            f encoding.y data extent

        Rect encoding ->
            extentWithPosition f encoding.y data extent

        Rule encoding ->
            extentWithPosition f encoding.y data extent

        Symbol encoding ->
            f encoding.y data extent

        Text encoding ->
            f encoding.y data extent

        Trail encoding ->
            f encoding.y data extent


extentDiscrete :
    { b | compareDomain : a -> a -> Order, field : Field.Field data a }
    -> List data
    -> List a
    -> List a
extentDiscrete channel data initialDomain =
    let
        field =
            channel.field

        compareDomain =
            channel.compareDomain

        scalarDomain =
            List.filterMap (Field.extract field) data

        aggregateDomain =
            List.filterMap
                (List.singleton >> Field.summarize field)
                data

        vectorDomain =
            Field.extractVector field data
                |> List.filterMap identity
    in
        List.concat [ initialDomain, scalarDomain, aggregateDomain, vectorDomain ]
            |> List.unique compareDomain


extentWithPosition :
    (PositionalChannel data domain -> a -> b -> b)
    -> Position.Position data domain
    -> a
    -> b
    -> b
extentWithPosition f position data extent =
    case position of
        Position.PrimarySecondary channel1 channel2 ->
            f channel1 data extent
                |> f channel2 data

        Position.PrimaryExtent channel _ ->
            f channel data extent

        Position.SecondaryExtent channel _ ->
            f channel data extent

        Position.CenterExtent channel _ ->
            f channel data extent


extentContinuous :
    { c | compareDomain : a -> a -> Order, field : Field.Field b a }
    -> List b
    -> ( Maybe a, Maybe a )
    -> ( Maybe a, Maybe a )
extentContinuous channel data initialDomain =
    let
        field =
            channel.field

        compareDomain =
            channel.compareDomain

        scalarDomain =
            List.foldl
                (\datum ( maybeMin, maybeMax ) ->
                    case Field.extract field datum of
                        Just dom ->
                            ( Just <| Maybe.maybe dom (minWith compareDomain dom) maybeMin
                            , Just <| Maybe.maybe dom (maxWith compareDomain dom) maybeMax
                            )

                        _ ->
                            ( maybeMin, maybeMax )
                )
                initialDomain
                data

        aggregateDomain =
            List.foldl
                (\datum ( maybeMin, maybeMax ) ->
                    case Field.summarize field [ datum ] of
                        Just dom ->
                            ( Just <| Maybe.maybe dom (minWith compareDomain dom) maybeMin
                            , Just <| Maybe.maybe dom (maxWith compareDomain dom) maybeMax
                            )

                        _ ->
                            ( maybeMin, maybeMax )
                )
                scalarDomain
                data

        vectorDomain =
            List.foldl
                (\maybeDom ( maybeMin, maybeMax ) ->
                    case maybeDom of
                        Just dom ->
                            ( Just <| Maybe.maybe dom (minWith compareDomain dom) maybeMin
                            , Just <| Maybe.maybe dom (maxWith compareDomain dom) maybeMax
                            )

                        _ ->
                            ( maybeMin, maybeMax )
                )
                aggregateDomain
                (Field.extractVector field data)
    in
        vectorDomain
