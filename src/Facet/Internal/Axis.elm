module Facet.Internal.Axis
    exposing
        ( Axis
        , isOrdinal
        , isContinuous
        , hasDomain
        , getTitle
        , getOrdinalScale
        , getContinuousScale
        , continuousX
        , continuousY
        , discreteX
        , discreteY
        , bandX
        , customBandX
        , bandY
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
        , labelAngle
        , subdivisions
        , format
        , orientTop
        , orientBottom
        , orientRight
        , orientLeft
        , scenegraphHorizontal
        , scenegraphVertical
        , calculateXAxisHeight
        , calculateYAxisWidth
        , verticalOrientation
        , horizontalOrientation
          -- , Orientation(..)
        , Vertical(..)
        , Horizontal(..)
        )

import Facet.Maybe.Extra as Maybe
import Facet.Scale as Scale exposing (Scale)
import Facet.Scenegraph as Scenegraph exposing (Scenegraph)
import Facet.Scenegraph.Mark as Mark
import Facet.Theme as Theme
import Facet.Theme as Theme
import Path.LowLevel exposing (SubPath, MoveTo(..), DrawTo(..), Mode(..))


type Axis orientation domain
    = ContinuousAxis orientation (ContinuousAxisInternal domain)
    | OrdinalAxis orientation (OrdinalAxisInternal domain)


type alias ContinuousAxisInternal domain =
    { scale : ( domain, domain ) -> ( Float, Float ) -> Scale domain Float
    , domain : Maybe ( domain, domain )
    , nice : Bool
    , title : Maybe String
    , formatDomain :
        domain -> String
        -- , orient : Orientation
    , numTicks : Maybe Int
    , subdivisions : Maybe Int
    , labelAngle : Maybe Float
    }


type alias OrdinalAxisInternal domain =
    { scale : List domain -> ( Float, Float ) -> Scale domain Float
    , domain : Maybe (List domain)
    , title : Maybe String
    , formatDomain :
        domain -> String
        -- , orient : Orientation
    , labelAngle : Maybe Float
    }


type alias AxisInternal a domain =
    { a
        | title : Maybe String
        , formatDomain :
            domain -> String
            -- , orient : Orientation
        , labelAngle : Maybe Float
    }


type Orientation
    = Horizontal Horizontal
    | Vertical Vertical


type Vertical
    = Top
    | Bottom


type Horizontal
    = Left
    | Right


hasDomain : Axis orientation domain -> Bool
hasDomain axis =
    case axis of
        ContinuousAxis _ a ->
            Maybe.isJust a.domain

        OrdinalAxis _ a ->
            Maybe.isJust a.domain


getTitle : Axis orientation domain -> Maybe String
getTitle axis =
    case axis of
        ContinuousAxis _ a ->
            a.title

        OrdinalAxis _ a ->
            a.title


isContinuous : Axis orientation domain -> Bool
isContinuous axis =
    case axis of
        ContinuousAxis _ _ ->
            True

        _ ->
            False


isOrdinal : Axis orientation domain -> Bool
isOrdinal axis =
    case axis of
        OrdinalAxis _ _ ->
            True

        _ ->
            False


orientation : Axis orientation domain -> orientation
orientation axis =
    case axis of
        ContinuousAxis orient _ ->
            orient

        OrdinalAxis orient _ ->
            orient


verticalOrientation : Axis Vertical domain -> Vertical
verticalOrientation axis =
    orientation axis


horizontalOrientation : Axis Horizontal domain -> Horizontal
horizontalOrientation axis =
    orientation axis



-- smart constructors ----------------------------------------------------------


continuousX :
    Maybe String
    -> (( a, a ) -> ( Float, Float ) -> Scale a Float)
    -> Axis Vertical a
continuousX title scale =
    ContinuousAxis Bottom <|
        ContinuousAxisInternal
            scale
            Nothing
            False
            title
            toString
            (Just 5)
            Nothing
            Nothing


continuousY :
    Maybe String
    -> (( a, a ) -> ( Float, Float ) -> Scale a Float)
    -> Axis Horizontal a
continuousY title scale =
    ContinuousAxis Left <|
        ContinuousAxisInternal
            scale
            Nothing
            False
            title
            toString
            (Just 5)
            Nothing
            Nothing


discreteX :
    Maybe String
    -> (List a -> ( Float, Float ) -> Scale a Float)
    -> Axis Vertical a
discreteX title scale =
    OrdinalAxis Bottom <|
        OrdinalAxisInternal
            scale
            Nothing
            title
            toString
            Nothing


discreteY :
    Maybe String
    -> (List a -> ( Float, Float ) -> Scale a Float)
    -> Axis Horizontal a
discreteY title scale =
    OrdinalAxis Left <|
        OrdinalAxisInternal
            scale
            Nothing
            title
            toString
            Nothing


bandX : Maybe String -> Axis Vertical comparableDomain
bandX title =
    discreteX title Scale.band


customBandX : Maybe String -> Axis Vertical anyDomain
customBandX title =
    discreteX title Scale.customBand


bandY : Maybe String -> Axis Horizontal comparableDomain
bandY title =
    discreteY title Scale.band


customBandY : Maybe String -> Axis Horizontal anyDomain
customBandY title =
    discreteY title Scale.customBand


linearX : Maybe String -> Axis Vertical Float
linearX title =
    continuousX title Scale.linear


linearY : Maybe String -> Axis Horizontal Float
linearY title =
    continuousY title Scale.linear


log10X : Maybe String -> Axis Vertical Float
log10X title =
    continuousX title Scale.log10


log10Y : Maybe String -> Axis Horizontal Float
log10Y title =
    continuousY title Scale.log10


sqrtX : Maybe String -> Axis Vertical Float
sqrtX title =
    continuousX title Scale.sqrt


sqrtY : Maybe String -> Axis Horizontal Float
sqrtY title =
    continuousY title Scale.sqrt



-- modifiers -------------------------------------------------------------------


continuousScale :
    (( domain, domain ) -> ( Float, Float ) -> Scale domain Float)
    -> Axis orientation domain
    -> Axis orientation domain
continuousScale scale axis =
    case axis of
        ContinuousAxis orientation a ->
            ContinuousAxis orientation { a | scale = scale }

        _ ->
            axis


ordinalScale :
    (List domain -> ( Float, Float ) -> Scale domain Float)
    -> Axis orientation domain
    -> Axis orientation domain
ordinalScale scale axis =
    case axis of
        OrdinalAxis orientation a ->
            OrdinalAxis orientation { a | scale = scale }

        _ ->
            axis


continuousDomain : ( domain, domain ) -> Axis orientation domain -> Axis orientation domain
continuousDomain domain axis =
    case axis of
        ContinuousAxis orientation a ->
            ContinuousAxis orientation { a | domain = Just domain }

        _ ->
            axis


ordinalDomain : List domain -> Axis orientation domain -> Axis orientation domain
ordinalDomain domain axis =
    case axis of
        OrdinalAxis orientation a ->
            OrdinalAxis orientation { a | domain = Just domain }

        _ ->
            axis


format : (domain -> String) -> Axis orientation domain -> Axis orientation domain
format format axis =
    case axis of
        ContinuousAxis orientation a ->
            ContinuousAxis orientation { a | formatDomain = format }

        OrdinalAxis orientation a ->
            OrdinalAxis orientation { a | formatDomain = format }


labelAngle : Float -> Axis orientation domain -> Axis orientation domain
labelAngle angle axis =
    case axis of
        ContinuousAxis orientation a ->
            ContinuousAxis orientation { a | labelAngle = Just angle }

        OrdinalAxis orientation a ->
            OrdinalAxis orientation { a | labelAngle = Just angle }


ticks : Int -> Axis orientation domain -> Axis orientation domain
ticks numTicks axis =
    case axis of
        ContinuousAxis orientation a ->
            ContinuousAxis orientation { a | numTicks = Just numTicks }

        _ ->
            axis


subdivisions : Int -> Axis orientation domain -> Axis orientation domain
subdivisions subdivisions axis =
    case axis of
        ContinuousAxis orientation a ->
            ContinuousAxis orientation { a | subdivisions = Just subdivisions }

        _ ->
            axis


orientLeft : Axis Horizontal domain -> Axis Horizontal domain
orientLeft axis =
    case axis of
        ContinuousAxis _ a ->
            ContinuousAxis Left a

        OrdinalAxis _ a ->
            OrdinalAxis Left a


orientRight : Axis Horizontal domain -> Axis Horizontal domain
orientRight axis =
    case axis of
        ContinuousAxis _ a ->
            ContinuousAxis Right a

        OrdinalAxis _ a ->
            OrdinalAxis Right a


orientTop : Axis Vertical domain -> Axis Vertical domain
orientTop axis =
    case axis of
        ContinuousAxis _ a ->
            ContinuousAxis Top a

        OrdinalAxis _ a ->
            OrdinalAxis Top a


orientBottom : Axis Vertical domain -> Axis Vertical domain
orientBottom axis =
    case axis of
        ContinuousAxis _ a ->
            ContinuousAxis Bottom a

        OrdinalAxis _ a ->
            OrdinalAxis Bottom a


getOrdinalScale :
    Maybe (List domain)
    -> ( Float, Float )
    -> Axis orientation domain
    -> Maybe (Scale domain Float)
getOrdinalScale defaultDomain range axis =
    case axis of
        OrdinalAxis _ axis ->
            Maybe.orElse defaultDomain axis.domain
                |> Maybe.map (\domain -> axis.scale domain range)

        _ ->
            Nothing


getContinuousScale :
    Maybe ( domain, domain )
    -> ( Float, Float )
    -> Axis orientation domain
    -> Maybe (Scale domain Float)
getContinuousScale defaultDomain range axis =
    case axis of
        ContinuousAxis _ axis ->
            Maybe.orElse defaultDomain axis.domain
                |> Maybe.map (\domain -> axis.scale domain range)

        _ ->
            Nothing



-- scenegraph generation -------------------------------------------------------


{-| TODO: find a way of determining the label lengths
    At the moment, the only way to do this for axes with continuous scales
    is to get the legend. However, at the point we want the axis size we
    don't know the range of  the scale yet since we haven't  calculated the
    axis size!
-}
calculateXAxisHeight : Theme.Axis -> Axis Vertical domain -> Float
calculateXAxisHeight theme axis =
    let
        fontSize =
            tick.label.font.fontSize

        labelOffset =
            tick.offset

        tick =
            theme.tick

        tickSize =
            List.maximum [ tick.sizeInner, tick.sizeOuter, tick.sizeMinor ]
                |> Maybe.withDefault 0

        axisOffset =
            theme.offset

        ( _, formatDomain, orientation, numTicks, subdivisions, labelAngle ) =
            axisInternal axis

        angle =
            norm <| Maybe.withDefault tick.labelAngle labelAngle
    in
        if angle == 0 || angle == 2 * pi then
            fontSize * 1.5 + labelOffset + tickSize + axisOffset
        else if angle == (pi / 2) || angle == (-pi / 2) then
            6.0 * fontSize
        else
            let
                labelSize =
                    4.0 * fontSize
            in
                labelSize * cos angle


calculateYAxisWidth : Theme.Axis -> Axis Horizontal domain -> Float
calculateYAxisWidth theme axis =
    let
        fontSize =
            tick.label.font.fontSize

        labelOffset =
            tick.offset

        tick =
            theme.tick

        tickSize =
            List.maximum [ tick.sizeInner, tick.sizeOuter, tick.sizeMinor ]
                |> Maybe.withDefault 0

        axisOffset =
            theme.offset

        ( _, formatDomain, orientation, numTicks, subdivisions, labelAngle ) =
            axisInternal axis

        angle =
            norm <| Maybe.withDefault tick.labelAngle labelAngle
    in
        if angle == pi / 2 || angle == -pi / 2 then
            fontSize * 1.5 + labelOffset + tickSize + axisOffset
        else
            let
                labelSize =
                    4.0 * fontSize
            in
                labelSize * cos angle


scenegraphVertical :
    Theme.Axis
    -> Float
    -> Float
    -> Scale domain Float
    -> Axis Vertical domain
    -> Scenegraph
scenegraphVertical theme width height scale axis =
    let
        ( title, formatDomain, orientation, numTicks, subdivisions, labelAngle ) =
            axisInternal axis

        range =
            ( 0, width )

        range0 =
            Tuple.first range

        range1 =
            Tuple.second range

        tickMark =
            xAxisTick height theme.tick labelAngle orientation

        actualTicks =
            Scale.legend scale (Maybe.withDefault 2 numTicks)

        ( ticks, labelLengths ) =
            actualTicks
                |> List.indexedMap
                    (\i ( dom, range ) ->
                        let
                            pos =
                                range / 2

                            label =
                                Just <| formatDomain dom

                            displayTickLine =
                                (not (pos == 0 || pos == width))
                        in
                            ( tickMark displayTickLine ( label, pos ), Maybe.maybe 0 String.length label )
                    )
                |> List.unzip

        labelSize =
            labelLengths
                |> List.maximum
                |> Maybe.withDefault 0
                |> toFloat
                >> (*) theme.tick.label.font.fontSize

        spacing =
            (max theme.tick.sizeInner 0) + theme.tick.padding

        svgpath =
            xAxisPath range0 range1 theme.tick orientation

        ( groupX, groupY ) =
            case orientation of
                {- x axis -}
                Top ->
                    ( 0, -theme.offset )

                Bottom ->
                    ( 0, height + theme.offset )

        path =
            Scenegraph.Path
                [ Mark.path 0 0 svgpath |> Mark.stroke theme.stroke ]

        axisGroup =
            Mark.group groupX 0 groupY 0 False

        size =
            calculateAxisSize theme
    in
        (Scenegraph.Group [ ( axisGroup, path :: ticks ) ])


scenegraphHorizontal :
    Theme.Axis
    -> Float
    -> Float
    -> Scale domain Float
    -> Axis Horizontal domain
    -> Scenegraph
scenegraphHorizontal theme width height scale axis =
    let
        ( title, formatDomain, orientation, numTicks, subdivisions, labelAngle ) =
            axisInternal axis

        range =
            ( 0, height )

        range0 =
            Tuple.first range

        range1 =
            Tuple.second range

        tickMark =
            yAxisTick width theme.tick labelAngle orientation

        actualTicks =
            Scale.legend scale (Maybe.withDefault 2 numTicks)

        ( ticks, labelLengths ) =
            actualTicks
                |> List.indexedMap
                    (\i ( dom, range ) ->
                        let
                            pos =
                                range / 2

                            label =
                                Just <| formatDomain dom

                            displayTickLine =
                                (not (pos == 0 || pos == height))
                        in
                            ( tickMark displayTickLine ( label, pos ), Maybe.maybe 0 String.length label )
                    )
                |> List.unzip

        labelSize =
            labelLengths
                |> List.maximum
                |> Maybe.withDefault 0
                |> toFloat
                >> (*) theme.tick.label.font.fontSize

        spacing =
            (max theme.tick.sizeInner 0) + theme.tick.padding

        svgpath =
            yAxisPath range0 range1 theme.tick orientation

        ( groupX, groupY ) =
            case orientation of
                Left ->
                    ( -theme.offset, 0 )

                Right ->
                    ( width + theme.offset, 0 )

        path =
            Scenegraph.Path
                [ Mark.path 0 0 svgpath |> Mark.stroke theme.stroke ]

        axisGroup =
            Mark.group groupX 0 groupY 0 False

        size =
            calculateAxisSize theme
    in
        (Scenegraph.Group [ ( axisGroup, path :: ticks ) ])



-- Ticks -----------------------------------------------------------------------


xAxisTick :
    Float
    -> Theme.Tick
    -> Maybe Float
    -> Vertical
    -> Bool
    -> ( Maybe String, Float )
    -> Scenegraph
xAxisTick height tickTheme labelAngle vPosition showTickLine ( maybeLabel, x ) =
    let
        font =
            tickTheme.label.font

        tickSize =
            case maybeLabel of
                Just _ ->
                    tickTheme.sizeInner

                _ ->
                    tickTheme.sizeMinor

        angle =
            norm <| Maybe.withDefault tickTheme.labelAngle labelAngle

        ( align, baseline ) =
            case vPosition of
                Bottom ->
                    if angle == 0 then
                        ( Mark.Center, Mark.Top )
                    else if angle < pi then
                        ( Mark.Right, Mark.Middle )
                    else
                        ( Mark.Left, Mark.Middle )

                Top ->
                    if angle == 0 then
                        ( Mark.Center, Mark.Bottom )
                    else if angle < pi then
                        ( Mark.Left, Mark.Middle )
                    else
                        ( Mark.Right, Mark.Middle )

        y =
            tickTheme.offset + tickTheme.sizeOuter + tickTheme.labelOffset

        --+ (font.fontSize / 2)
        ( maybeRuleMark, maybeTextMark ) =
            case vPosition of
                Bottom ->
                    ( if showTickLine then
                        Mark.rule x x tickTheme.offset (tickTheme.offset + tickSize)
                            |> Mark.stroke tickTheme.stroke
                            |> List.singleton
                            |> Scenegraph.Rule
                            |> Just
                      else
                        Nothing
                    , Maybe.map
                        (\label ->
                            Mark.text x y label
                                |> Mark.align align
                                |> Mark.baseline baseline
                                |> Mark.angle angle
                                |> Mark.font font
                                |> Mark.stroke tickTheme.label.stroke
                                |> Mark.fill tickTheme.label.fill
                                |> List.singleton
                                |> Scenegraph.Text
                        )
                        maybeLabel
                    )

                Top ->
                    ( if showTickLine then
                        Mark.rule x x -(tickTheme.offset + tickSize) -tickTheme.offset
                            |> Mark.stroke tickTheme.stroke
                            |> List.singleton
                            |> Scenegraph.Rule
                            |> Just
                      else
                        Nothing
                    , Maybe.map
                        (\label ->
                            Mark.text x -y label
                                |> Mark.font font
                                |> Mark.align align
                                |> Mark.baseline baseline
                                |> Mark.angle angle
                                |> Mark.stroke tickTheme.label.stroke
                                |> Mark.fill tickTheme.label.fill
                                |> List.singleton
                                |> Scenegraph.Text
                        )
                        maybeLabel
                    )

        elems =
            List.filterMap identity [ maybeRuleMark, maybeTextMark ]

        groupInfo =
            Mark.group x 0 0 0 False
    in
        Scenegraph.Group [ ( groupInfo, elems ) ]


yAxisTick :
    Float
    -> Theme.Tick
    -> Maybe Float
    -> Horizontal
    -> Bool
    -> ( Maybe String, Float )
    -> Scenegraph
yAxisTick width tickTheme labelAngle hPosition showTickLine ( maybeLabel, y ) =
    let
        font =
            tickTheme.label.font

        tickSize =
            case maybeLabel of
                Just _ ->
                    tickTheme.sizeInner

                _ ->
                    tickTheme.sizeMinor

        angle =
            norm <| Maybe.withDefault tickTheme.labelAngle labelAngle

        ( align, baseline ) =
            case hPosition of
                Right ->
                    if angle == pi / 2 || angle == -pi / 2 then
                        ( Mark.Center, Mark.Bottom )
                    else if angle < pi then
                        ( Mark.Right, Mark.Middle )
                    else
                        ( Mark.Left, Mark.Middle )

                Left ->
                    if angle == pi / 2 || angle == -pi / 2 then
                        ( Mark.Center, Mark.Top )
                    else if angle < pi then
                        ( Mark.Left, Mark.Middle )
                    else
                        ( Mark.Right, Mark.Middle )

        ( maybeRuleMark, maybeTextMark ) =
            case hPosition of
                Left ->
                    ( if showTickLine then
                        Mark.rule -(tickTheme.offset + tickSize) -tickTheme.offset y y
                            |> Mark.stroke tickTheme.stroke
                            |> List.singleton
                            |> Scenegraph.Rule
                            |> Just
                      else
                        Nothing
                    , Maybe.map
                        (\label ->
                            Mark.text -(tickTheme.offset + tickTheme.sizeOuter + tickTheme.labelOffset) y label
                                |> Mark.font tickTheme.label.font
                                |> Mark.align align
                                |> Mark.baseline baseline
                                |> Mark.angle angle
                                |> Mark.stroke tickTheme.label.stroke
                                |> Mark.fill tickTheme.label.fill
                                |> List.singleton
                                |> Scenegraph.Text
                        )
                        maybeLabel
                    )

                Right ->
                    ( if showTickLine then
                        Mark.rule tickTheme.offset (tickTheme.offset + tickSize) y y
                            |> Mark.stroke tickTheme.stroke
                            |> List.singleton
                            |> Scenegraph.Rule
                            |> Just
                      else
                        Nothing
                    , Maybe.map
                        (\label ->
                            Mark.text (tickTheme.offset + tickTheme.sizeOuter + tickTheme.labelOffset) y label
                                |> Mark.font tickTheme.label.font
                                |> Mark.align align
                                |> Mark.baseline baseline
                                |> Mark.angle angle
                                |> Mark.stroke tickTheme.label.stroke
                                |> Mark.fill tickTheme.label.fill
                                |> List.singleton
                                |> Scenegraph.Text
                        )
                        maybeLabel
                    )

        elems =
            List.filterMap identity [ maybeRuleMark, maybeTextMark ]

        groupInfo =
            Mark.group 0 0 y 0 False
    in
        Scenegraph.Group [ ( groupInfo, elems ) ]



-- Paths -----------------------------------------------------------------------


xAxisPath :
    Float
    -> Float
    -> Theme.Tick
    -> Vertical
    -> SubPath
xAxisPath range0 range1 tickTheme vPosition =
    let
        sizeOuter =
            case vPosition of
                Top ->
                    -1.0 * tickTheme.sizeOuter

                _ ->
                    tickTheme.sizeOuter
    in
        SubPath
            (MoveTo Absolute ( range0, sizeOuter ))
            [ LineTo Absolute
                [ ( range0, 0.5 )
                , ( range1, 0.5 )
                , ( range1, sizeOuter )
                ]
            ]


yAxisPath :
    Float
    -> Float
    -> Theme.Tick
    -> Horizontal
    -> SubPath
yAxisPath range0 range1 tickTheme hPosition =
    let
        sizeOuter =
            case hPosition of
                Left ->
                    -1.0 * tickTheme.sizeOuter

                _ ->
                    tickTheme.sizeOuter
    in
        SubPath
            (MoveTo Absolute ( sizeOuter, range0 ))
            [ LineTo Absolute
                [ ( 0.5, range0 )
                , ( 0.5, range1 )
                , ( sizeOuter, range1 )
                ]
            ]



-- Helpers ---------------------------------------------------------------------


calculateAxisSize : Theme.Axis -> Float
calculateAxisSize axisTheme =
    let
        fontSize =
            tick.label.font.fontSize * 1.5

        labelOffset =
            tick.offset

        tick =
            axisTheme.tick

        tickSize =
            List.maximum [ tick.sizeInner, tick.sizeOuter, tick.sizeMinor ]
                |> Maybe.withDefault 0

        axisOffset =
            axisTheme.offset
    in
        fontSize + labelOffset + tickSize + axisOffset


axisInternal :
    Axis orientation domain
    -> ( Maybe String, domain -> String, orientation, Maybe Int, Maybe Int, Maybe Float )
axisInternal axis =
    case axis of
        ContinuousAxis orientation a ->
            ( a.title
            , a.formatDomain
            , orientation
            , a.numTicks
            , a.subdivisions
            , a.labelAngle
            )

        OrdinalAxis orientation a ->
            ( a.title
            , a.formatDomain
            , orientation
            , Nothing
            , Nothing
            , a.labelAngle
            )


mapBoth : (a -> b) -> ( a, a ) -> ( b, b )
mapBoth f ( x, y ) =
    ( f x, f y )


norm : Float -> Float
norm rads =
    rads - turns (toFloat (floor (rads / (2 * pi))))
