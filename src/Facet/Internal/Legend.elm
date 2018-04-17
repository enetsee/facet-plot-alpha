module Facet.Internal.Legend exposing (Legend, LegendSpec, legend, isEmpty, scenegraph)

import Color exposing (Color)
import Facet.Internal.Field as Field exposing (Field)
import Facet.Scale as Scale exposing (Scale)
import Facet.Scenegraph as Scenegraph exposing (Scenegraph)
import Facet.Scenegraph.Shape as Shape
import Facet.Scenegraph.Stroke as Stroke exposing (StrokeDash)
import Facet.Scenegraph.Mark as Mark
import Facet.Scenegraph.Shape exposing (Shape)
import Facet.Theme as Theme
import Facet.Maybe.Extra as Maybe


type alias Legend range =
    { title : Maybe String
    , items : List ( String, range )
    }


isEmpty : Legend range -> Bool
isEmpty { items } =
    List.isEmpty items


legend : (domain -> String) -> Field data domain -> Scale domain range -> Int -> Legend range
legend formatDomain field scale numItems =
    { title = Field.fieldName field
    , items =
        Scale.legend scale numItems
            |> List.map (\( dom, range ) -> ( formatDomain dom, range ))
    }


type alias LegendSpec =
    { fillColor : Maybe (Legend Color)
    , fillOpacity : Maybe (Legend Float)
    , strokeColor : Maybe (Legend Color)
    , strokeOpacity : Maybe (Legend Float)
    , strokeWidth : Maybe (Legend Float)
    , strokeDash : Maybe (Legend StrokeDash)
    , angle : Maybe (Legend Float)
    , shape : Maybe (Legend Shape)
    , cornerRadius : Maybe (Legend Float)
    , size : Maybe (Legend Float)
    , width : Maybe (Legend Float)
    }



-- SCENEGRAPH ------------------------------------------------------------------


scenegraph : Theme.Legend -> LegendSpec -> List ( Scenegraph, ( Float, Float ) )
scenegraph theme legendSpec =
    [ Maybe.map (fillColorLegend theme) legendSpec.fillColor
    , Maybe.map (fillOpacityLegend theme) legendSpec.fillOpacity
    , Maybe.map (strokeColorLegend theme) legendSpec.strokeColor
    , Maybe.map (strokeOpacityLegend theme) legendSpec.strokeOpacity
    , Maybe.map (strokeWidthLegend theme) legendSpec.strokeWidth
    , Maybe.map (strokeDashLegend theme) legendSpec.strokeDash
    , Maybe.map (shapeLegend theme) legendSpec.shape
    , Maybe.map (angleLegend theme) legendSpec.angle
    , Maybe.map (sizeLegend theme) legendSpec.size
    , Maybe.map (cornerRadiusLegend theme) legendSpec.cornerRadius
    ]
        |> List.filterMap identity



-- Mark size / width -----------------------------------------------------------


{-| Legend for data encoded as the size of a visual mark
-}
sizeLegend : Theme.Legend -> Legend Float -> ( Scenegraph, ( Float, Float ) )
sizeLegend theme ({ items } as legend) =
    let
        maxSize =
            items
                |> List.map Tuple.second
                |> List.maximum
                |> Maybe.map (sqrt >> (*) 1.2)
    in
        legendHelper theme legend maxSize sizeItem


sizeItem : Theme.Label -> Theme.Padding -> Float -> Int -> ( String, Float ) -> Scenegraph
sizeItem labelTheme padding itemHeight i ( labelText, size ) =
    let
        ypos =
            itemHeight * (toFloat i)

        sizeMark =
            Mark.symbol (padding.left + (itemHeight / 2)) (padding.top + (itemHeight / 3)) size Shape.Circle
                |> Mark.fillColor Color.blue
                |> List.singleton
                |> Scenegraph.Symbol

        labelMark =
            labelText
                |> label labelTheme (padding.left * 2 + itemHeight + padding.right) padding.top
                |> Mark.align Mark.Right
                |> Mark.baseline Mark.Top
                |> Mark.relativePosition 0 0.33
                |> List.singleton
                |> Scenegraph.Text

        groupInfo =
            Mark.group 0 0 ypos 0 False
    in
        Scenegraph.Group [ ( groupInfo, [ labelMark, sizeMark ] ) ]



-- Mark angle ------------------------------------------------------------------


{-| Legend for data encoded as the rotation angle of a visual mark
-}
angleLegend : Theme.Legend -> Legend Float -> ( Scenegraph, ( Float, Float ) )
angleLegend theme legend =
    legendHelper theme legend Nothing angleItem


angleItem : Theme.Label -> Theme.Padding -> Float -> Int -> ( String, Float ) -> Scenegraph
angleItem labelTheme padding itemHeight i ( labelText, angle ) =
    let
        ypos =
            itemHeight * (toFloat i)

        markSize =
            (itemHeight - padding.top - padding.bottom)

        angleMark =
            Mark.symbol (padding.left + (markSize / 2)) (padding.top + (markSize / 2)) (markSize * markSize) Shape.Arrow
                |> Mark.fillColor Color.blue
                |> Mark.angle angle
                |> List.singleton
                |> Scenegraph.Symbol

        labelMark =
            labelText
                |> label labelTheme (padding.left * 2 + markSize + padding.right) padding.top
                |> Mark.align Mark.Right
                |> Mark.baseline Mark.Top
                |> Mark.relativePosition 0 0.0
                |> List.singleton
                |> Scenegraph.Text

        groupInfo =
            Mark.group 0 0 ypos 0 False
    in
        Scenegraph.Group [ ( groupInfo, [ labelMark, angleMark ] ) ]



-- Mark shape ------------------------------------------------------------------


{-| Legend for data encoded as the rotation angle of a visual mark
-}
shapeLegend : Theme.Legend -> Legend Shape -> ( Scenegraph, ( Float, Float ) )
shapeLegend theme legend =
    legendHelper theme legend Nothing shapeItem


shapeItem : Theme.Label -> Theme.Padding -> Float -> Int -> ( String, Shape ) -> Scenegraph
shapeItem labelTheme padding itemHeight i ( labelText, shape ) =
    let
        ypos =
            itemHeight * (toFloat i)

        markSize =
            (itemHeight - padding.top - padding.bottom)

        shapeMark =
            Mark.symbol (padding.left + (markSize / 2)) (padding.top + (markSize / 2)) markSize shape
                |> Mark.strokeColor Color.blue
                |> Mark.strokeWidth 1
                |> List.singleton
                |> Scenegraph.Symbol

        labelMark =
            labelText
                |> label labelTheme (padding.left * 2 + markSize + padding.right) padding.top
                |> Mark.align Mark.Right
                |> Mark.baseline Mark.Top
                |> Mark.relativePosition 0 0.0
                |> List.singleton
                |> Scenegraph.Text

        groupInfo =
            Mark.group 0 0 ypos 0 False
    in
        Scenegraph.Group [ ( groupInfo, [ labelMark, shapeMark ] ) ]



-- Mark corner radius  ---------------------------------------------------------


{-| Legend for data encoded as the corner radius of a visual mark... for some
    reason?!
-}
cornerRadiusLegend : Theme.Legend -> Legend Float -> ( Scenegraph, ( Float, Float ) )
cornerRadiusLegend theme legend =
    legendHelper theme legend Nothing cornerRadiusItem


cornerRadiusItem : Theme.Label -> Theme.Padding -> Float -> Int -> ( String, Float ) -> Scenegraph
cornerRadiusItem labelTheme padding itemHeight i ( labelText, radius ) =
    let
        ypos =
            itemHeight * (toFloat i)

        markSize =
            (itemHeight - padding.top - padding.bottom)

        rectMark =
            Mark.rect padding.left (padding.left + markSize) padding.top (padding.top + markSize)
                |> Mark.cornerRadius radius
                |> Mark.strokeColor Color.blue
                |> Mark.strokeWidth 1
                |> List.singleton
                |> Scenegraph.Rect

        labelMark =
            labelText
                |> label labelTheme (padding.left * 2 + markSize + padding.right) padding.top
                |> Mark.align Mark.Right
                |> Mark.baseline Mark.Top
                |> Mark.relativePosition 0 0.0
                |> List.singleton
                |> Scenegraph.Text

        groupInfo =
            Mark.group 0 0 ypos 0 False
    in
        Scenegraph.Group [ ( groupInfo, [ labelMark, rectMark ] ) ]



-- Mark fill  -------------------------------------------------------------------


{-| Legend for data encoded as the fill color of a visual mark
-}
fillColorLegend : Theme.Legend -> Legend Color -> ( Scenegraph, ( Float, Float ) )
fillColorLegend theme legend =
    legendHelper theme legend Nothing fillColorItem


fillColorItem : Theme.Label -> Theme.Padding -> Float -> Int -> ( String, Color ) -> Scenegraph
fillColorItem labelTheme padding itemHeight i ( labelText, color ) =
    let
        ypos =
            itemHeight * (toFloat i)

        markSize =
            (itemHeight - padding.top - padding.bottom)

        colorMark =
            Mark.rect padding.left (padding.left + markSize) padding.top (padding.top + markSize)
                |> Mark.fillColor color
                |> List.singleton
                |> Scenegraph.Rect

        labelMark =
            labelText
                |> label labelTheme (padding.left * 2 + markSize + padding.right) padding.top
                |> Mark.align Mark.Right
                |> Mark.baseline Mark.Top
                |> Mark.relativePosition 0 0.0
                |> List.singleton
                |> Scenegraph.Text

        groupInfo =
            Mark.group 0 0 ypos 0 False
    in
        Scenegraph.Group [ ( groupInfo, [ labelMark, colorMark ] ) ]


{-| Legend for data encoded as the fill color of a visual mark
-}
fillOpacityLegend : Theme.Legend -> Legend Float -> ( Scenegraph, ( Float, Float ) )
fillOpacityLegend theme legend =
    legendHelper theme legend Nothing fillOpacityItem


fillOpacityItem : Theme.Label -> Theme.Padding -> Float -> Int -> ( String, Float ) -> Scenegraph
fillOpacityItem labelTheme padding itemHeight i ( labelText, opacity ) =
    let
        ypos =
            itemHeight * (toFloat i)

        markSize =
            (itemHeight - padding.top - padding.bottom)

        opacityMark =
            Mark.rect padding.left (padding.left + markSize) padding.top (padding.top + markSize)
                |> Mark.fillColor Color.black
                |> Mark.fillOpacity opacity
                |> Mark.strokeColor Color.black
                |> Mark.strokeWidth 1
                |> List.singleton
                |> Scenegraph.Rect

        labelMark =
            labelText
                |> label labelTheme (padding.left * 2 + markSize + padding.right) padding.top
                |> Mark.align Mark.Right
                |> Mark.baseline Mark.Top
                |> Mark.relativePosition 0 0.0
                |> List.singleton
                |> Scenegraph.Text

        groupInfo =
            Mark.group 0 0 ypos 0 False
    in
        Scenegraph.Group [ ( groupInfo, [ labelMark, opacityMark ] ) ]



-- Mark stroke -----------------------------------------------------------------


{-| Legend for data encoded as the fill color of a visual mark
-}
strokeColorLegend : Theme.Legend -> Legend Color -> ( Scenegraph, ( Float, Float ) )
strokeColorLegend theme legend =
    legendHelper theme legend Nothing strokeColorItem


strokeColorItem : Theme.Label -> Theme.Padding -> Float -> Int -> ( String, Color ) -> Scenegraph
strokeColorItem labelTheme padding itemHeight i ( labelText, color ) =
    let
        ypos =
            itemHeight * (toFloat i)

        markSize =
            (itemHeight - padding.top - padding.bottom)

        rectMark =
            Mark.rect padding.left (padding.left + markSize) padding.top (padding.top + markSize)
                |> Mark.strokeColor Color.lightGray
                |> List.singleton
                |> Scenegraph.Rect

        colorMark =
            Mark.rule padding.left (padding.left + markSize) (padding.top + (markSize / 2)) (padding.top + (markSize / 2))
                |> Mark.strokeColor color
                |> Mark.strokeWidth 2.0
                |> List.singleton
                |> Scenegraph.Rule

        labelMark =
            labelText
                |> label labelTheme (padding.left * 2 + markSize + padding.right) padding.top
                |> Mark.align Mark.Right
                |> Mark.baseline Mark.Top
                |> Mark.relativePosition 0 0.0
                |> List.singleton
                |> Scenegraph.Text

        groupInfo =
            Mark.group 0 0 ypos 0 False
    in
        Scenegraph.Group [ ( groupInfo, [ labelMark, rectMark, colorMark ] ) ]


{-| Legend for data encoded as the stroke opacity of a visual mark
-}
strokeOpacityLegend : Theme.Legend -> Legend Float -> ( Scenegraph, ( Float, Float ) )
strokeOpacityLegend theme legend =
    legendHelper theme legend Nothing strokeOpacityItem


strokeOpacityItem : Theme.Label -> Theme.Padding -> Float -> Int -> ( String, Float ) -> Scenegraph
strokeOpacityItem labelTheme padding itemHeight i ( labelText, opacity ) =
    let
        ypos =
            itemHeight * (toFloat i)

        markSize =
            (itemHeight - padding.top - padding.bottom)

        rectMark =
            Mark.rect padding.left (padding.left + markSize) padding.top (padding.top + markSize)
                |> Mark.strokeColor Color.lightGray
                |> List.singleton
                |> Scenegraph.Rect

        strokeMark =
            Mark.rule padding.left (padding.left + markSize) (padding.top + (markSize / 2)) (padding.top + (markSize / 2))
                |> Mark.strokeColor Color.blue
                |> Mark.strokeWidth 2.0
                |> Mark.strokeOpacity opacity
                |> List.singleton
                |> Scenegraph.Rule

        labelMark =
            labelText
                |> label labelTheme (padding.left * 2 + markSize + padding.right) padding.top
                |> Mark.align Mark.Right
                |> Mark.baseline Mark.Top
                |> Mark.relativePosition 0 0.0
                |> List.singleton
                |> Scenegraph.Text

        groupInfo =
            Mark.group 0 0 ypos 0 False
    in
        Scenegraph.Group [ ( groupInfo, [ labelMark, rectMark, strokeMark ] ) ]


{-| Legend for data encoded as the stroke width of a visual mark
-}
strokeWidthLegend : Theme.Legend -> Legend Float -> ( Scenegraph, ( Float, Float ) )
strokeWidthLegend theme legend =
    legendHelper theme legend Nothing strokeWidthItem


strokeWidthItem : Theme.Label -> Theme.Padding -> Float -> Int -> ( String, Float ) -> Scenegraph
strokeWidthItem labelTheme padding itemHeight i ( labelText, width ) =
    let
        ypos =
            itemHeight * (toFloat i)

        markSize =
            (itemHeight - padding.top - padding.bottom)

        rectMark =
            Mark.rect padding.left (padding.left + markSize) padding.top (padding.top + markSize)
                |> Mark.strokeColor Color.lightGray
                |> List.singleton
                |> Scenegraph.Rect

        strokeMark =
            Mark.rule padding.left (padding.left + markSize) (padding.top + (markSize / 2)) (padding.top + (markSize / 2))
                |> Mark.strokeColor Color.blue
                |> Mark.strokeWidth width
                |> List.singleton
                |> Scenegraph.Rule

        labelMark =
            labelText
                |> label labelTheme (padding.left * 2 + markSize + padding.right) padding.top
                |> Mark.align Mark.Right
                |> Mark.baseline Mark.Top
                |> Mark.relativePosition 0 0.0
                |> List.singleton
                |> Scenegraph.Text

        groupInfo =
            Mark.group 0 0 ypos 0 False
    in
        Scenegraph.Group [ ( groupInfo, [ labelMark, rectMark, strokeMark ] ) ]


{-| Legend for data encoded as the stroke dash of a visual mark
-}
strokeDashLegend : Theme.Legend -> Legend StrokeDash -> ( Scenegraph, ( Float, Float ) )
strokeDashLegend theme legend =
    legendHelper theme legend Nothing strokeDashItem


strokeDashItem : Theme.Label -> Theme.Padding -> Float -> Int -> ( String, StrokeDash ) -> Scenegraph
strokeDashItem labelTheme padding itemHeight i ( labelText, strokeDash ) =
    let
        ypos =
            itemHeight * (toFloat i)

        markSize =
            (itemHeight - padding.top - padding.bottom)

        rectMark =
            Mark.rect padding.left (padding.left + markSize) padding.top (padding.top + markSize)
                |> Mark.strokeColor Color.lightGray
                |> List.singleton
                |> Scenegraph.Rect

        strokeMark =
            Mark.rule padding.left (padding.left + markSize) (padding.top + (markSize / 2)) (padding.top + (markSize / 2))
                |> Mark.strokeColor Color.blue
                |> Mark.strokeWidth 2
                |> Mark.strokeDash strokeDash
                |> List.singleton
                |> Scenegraph.Rule

        labelMark =
            labelText
                |> label labelTheme (padding.left * 2 + markSize + padding.right) padding.top
                |> Mark.align Mark.Right
                |> Mark.baseline Mark.Top
                |> Mark.relativePosition 0 0.0
                |> List.singleton
                |> Scenegraph.Text

        groupInfo =
            Mark.group 0 0 ypos 0 False
    in
        Scenegraph.Group [ ( groupInfo, [ labelMark, rectMark, strokeMark ] ) ]



-- Helpers ---------------------------------------------------------------------


legendHelper :
    Theme.Legend
    -> Legend a
    -> Maybe Float
    -> (Theme.Label -> Theme.Padding -> Float -> Int -> ( String, a ) -> Scenegraph)
    -> ( Scenegraph, ( Float, Float ) )
legendHelper theme { title, items } maybeItemSize mkItem =
    let
        titleText =
            title
                |> Maybe.withDefault "unnamed"

        maxLabelWidth =
            items
                |> List.map (Tuple.first >> String.length)
                |> List.maximum
                |> Maybe.withDefault 0
                |> (toFloat >> ((*) theme.title.font.fontSize) >> ((*) 0.66))

        titleWidth =
            (toFloat <| String.length titleText) * theme.title.font.fontSize * 0.66

        markWidth =
            itemHeight + itemPadding.left + itemPadding.right + maxLabelWidth

        totalWidth =
            (max titleWidth markWidth) + padding.left + padding.right

        totalHeight =
            (toFloat (1 + nItems)) * itemHeight + padding.top + padding.bottom

        padding =
            theme.padding

        itemPadding =
            theme.itemPadding

        itemHeight =
            Maybe.maybe theme.itemHeight (\size -> size + itemPadding.top + itemPadding.bottom) maybeItemSize

        nItems =
            List.length items

        background =
            Mark.rect 0 totalWidth 0 totalHeight
                |> Mark.stroke theme.stroke
                |> Mark.fill theme.fill
                |> List.singleton
                |> Scenegraph.Rect

        titleMark =
            titleText
                |> label theme.title padding.left padding.top
                |> Mark.baseline Mark.Top
                |> Mark.align Mark.Right
                |> List.singleton
                |> Scenegraph.Text

        itemMark =
            items
                |> List.indexedMap
                    (mkItem theme.itemLabel itemPadding itemHeight)
                |> (,) (Mark.group padding.left 0 itemHeight 0 False)
                |> List.singleton
                |> Scenegraph.Group

        groupInfo =
            Mark.group 0 totalWidth 0 totalHeight False
    in
        ( Scenegraph.Group [ ( groupInfo, [ background, titleMark, itemMark ] ) ]
        , ( totalWidth, totalHeight )
        )


label : Theme.Label -> Float -> Float -> String -> Mark.Text
label labelTheme x y text =
    Mark.text x y text
        |> Mark.font labelTheme.font
        |> Mark.stroke labelTheme.stroke
        |> Mark.fill labelTheme.fill
