module Facet.Plot
    exposing
        ( Plot
        , PlotError(..)
        , plot
        , compile
        , compileWithTheme
        , size
        , width
        , height
        , facetGrid
        , facetRow
        , facetRowWrap
        , facetColumn
        , facetColumnWrap
        , facetRowFormat
        , facetColumnFormat
        , layer
        , xAxis
        , yAxis
        )

{-|
@docs Plot, PlotError

@docs plot, size, width, height

@docs layer

@docs facetGrid, facetRow, facetRowWrap, facetColumn, facetColumnWrap, facetRowFormat,facetColumnFormat

@docs xAxis, yAxis

@docs compile, compileWithTheme
-}

import Facet.Internal.Axis as AxisInternal exposing (Vertical(..), Horizontal(..))
import Facet.Axis as Axis exposing (Axis)
import Facet.Internal.Encoding as Encoding exposing (Encoding)
import Facet.Internal.Field as Field exposing (Field)
import Facet.Internal.Legend as Legend
import Facet.Maybe.Extra as Maybe
import Facet.List.Extra as List
import Facet.Scale as Scale
import Facet.Scenegraph as Scenegraph exposing (Scenegraph, ViewBox)
import Facet.Scenegraph.Mark as Mark
import Facet.Theme as Theme exposing (Theme)
import Facet.Helpers exposing (compareMaybe)


{-| A `Plot` allows you to combine several layers of `Encodings` along with
    the corresponding `Legends` and `Axis`.

    In addition, you can specify how the plot should be facetted to create
    [small multiples](https://en.wikipedia.org/wiki/Small_multiple).
-}
type Plot data xdomain ydomain facetRow facetColumn
    = Plot (PlotSpec data xdomain ydomain facetRow facetColumn)


type alias PlotSpec data xdomain ydomain facetRow facetColumn =
    { title : Maybe String
    , layers : List (Encoding data xdomain ydomain)
    , xAxis : Maybe (Axis.Axis Vertical xdomain)
    , yAxis : Maybe (Axis.Axis Horizontal ydomain)
    , facet : Maybe (Facet data facetRow facetColumn)
    , width : Maybe Float
    , height : Maybe Float
    , legendItems : Maybe Int
    }


type Facet data facetRow facetColumn
    = Grid (FacetDetails data facetRow) (FacetDetails data facetColumn)
    | Row (FacetDetails data facetRow) (Maybe Int)
    | Column (FacetDetails data facetColumn) (Maybe Int)


type alias FacetDetails data facet =
    { compareWith : facet -> facet -> Order
    , format : facet -> String
    , field : Field data facet
    }


{-| Reasons why plot compilation may have failed
-}
type PlotError
    = NoXAxis
    | NoYAxis


{-| Plot measurements needed for layout
-}
type alias PlotMeasurements =
    { totalWidth : Float
    , totalHeight : Float
    , effectiveWidth : Float
    , effectiveHeight : Float
    , legendWidth : Float
    , legendHeight : Float
    , availableWidth : Float
    , availableHeight : Float
    , xAxisHeight : Float
    , xAxisTitleHeight : Float
    , yAxisWidth : Float
    , yAxisTitleWidth : Float
    , plotTitleHeight : Float
    , allPanelWidth : Float
    , allPanelHeight : Float
    , panelWidth : Float
    , panelHeight : Float
    }


{-| Values and functions derived from plot measurements needed for layout
-}
type alias PlotLayout xdomain ydomain =
    { nRows : Int
    , nCols : Int
    , facetTitleHeight : Float
    , totalFacetTitleHeight : Float
    , facetTitleWidth : Float
    , totalFacetTitleWidth : Float
    , xAxis : PlotMeasurements -> Scale.Scale xdomain Float -> Int -> Int -> Maybe Scenegraph
    , xAxisOrientation : Vertical
    , yAxis : PlotMeasurements -> Scale.Scale ydomain Float -> Int -> Int -> Maybe Scenegraph
    , yAxisOrientation : Horizontal
    , facetColumnLabel : PlotMeasurements -> Int -> Int -> Float -> Float -> String -> Maybe Scenegraph
    , facetRowLabel : PlotMeasurements -> Int -> Int -> Float -> Float -> String -> Maybe Scenegraph
    , panelX : PlotMeasurements -> Int -> Float
    , panelY : PlotMeasurements -> Int -> Float
    }



{--Create and `modify` plots --------------------------------------------------}


{-| An empty `Plot` with an optional title
-}
plot : Maybe String -> Plot data xdomain ydomain facetRow facetColumn
plot title =
    Plot <|
        PlotSpec title [] Nothing Nothing Nothing Nothing Nothing Nothing



-- Set user-space dimensions ---------------------------------------------------


{-| Specify the user-space width and height of a plot
-}
size : Float -> Float -> Plot data xdomain ydomain facetRow facetColumn -> Plot data xdomain ydomain facetRow facetColumn
size width height (Plot plotSpec) =
    Plot <| { plotSpec | width = Just width, height = Just height }


{-| Specify the user-space width of a plot
-}
width : Float -> Plot data xdomain ydomain facetRow facetColumn -> Plot data xdomain ydomain facetRow facetColumn
width width (Plot plotSpec) =
    Plot <| { plotSpec | width = Just width }


{-| Specify the user-space height of a plot
-}
height : Float -> Plot data xdomain ydomain facetRow facetColumn -> Plot data xdomain ydomain facetRow facetColumn
height height (Plot plotSpec) =
    Plot <| { plotSpec | height = Just height }



-- Add axes --------------------------------------------------------------------


{-| -}
xAxis : Axis Vertical xdomain -> Plot data xdomain ydomain facetRow facetColumn -> Plot data xdomain ydomain facetRow facetColumn
xAxis xAxis (Plot plotSpec) =
    Plot <|
        { plotSpec | xAxis = Just xAxis }


{-| -}
yAxis : Axis Horizontal ydomain -> Plot data xdomain ydomain facetRow facetColumn -> Plot data xdomain ydomain facetRow facetColumn
yAxis yAxis (Plot plotSpec) =
    Plot <|
        { plotSpec | yAxis = Just yAxis }



-- Add encoding layer ----------------------------------------------------------


{-| -}
layer :
    Encoding data xdomain ydomain
    -> Plot data xdomain ydomain facetRow facetColumn
    -> Plot data xdomain ydomain facetRow facetColumn
layer encoding (Plot plotSpec) =
    Plot <|
        { plotSpec | layers = encoding :: plotSpec.layers }



-- Facet plot ------------------------------------------------------------------


{-| Generate a grid of small multiples using the same scale and axes
    , allowing them to be easily compared.
-}
facetGrid :
    { r
        | row : Field data comparableRow
        , column : Field data comparableColumn
    }
    -> Plot data xdomain ydomain a b
    -> Plot data xdomain ydomain comparableRow comparableColumn
facetGrid { row, column } (Plot plotSpec) =
    let
        facetRow =
            FacetDetails compare toString row

        facetColumn =
            FacetDetails compare toString column
    in
        Plot <|
            { plotSpec | facet = Just <| Grid facetRow facetColumn }


{-| Generate a row of small multiples using the same scale and axes, allowing
    them to be easily compared.
-}
facetRow :
    { r | row : Field data comparableRow }
    -> Plot data xdomain ydomain a b
    -> Plot data xdomain ydomain comparableRow b
facetRow { row } (Plot plotSpec) =
    let
        facetRow =
            FacetDetails compare toString row
    in
        Plot <|
            { plotSpec | facet = Just <| Row facetRow Nothing }


{-| Generate a column of small multiples using the same scale and axes, allowing
    them to be easily compared.
-}
facetColumn :
    { r | column : Field data comparableColumn }
    -> Plot data xdomain ydomain a b
    -> Plot data xdomain ydomain a comparableColumn
facetColumn { column } (Plot plotSpec) =
    let
        facetColumn =
            FacetDetails compare toString column
    in
        Plot <|
            { plotSpec | facet = Just <| Column facetColumn Nothing }


{-| Generate rows of small multiples, with a maximum length, using the same
    scale and axes, allowing them to be easily compared.
-}
facetRowWrap :
    { r | row : Field data comparableRow, maxPerRow : Int }
    -> Plot data xdomain ydomain a b
    -> Plot data xdomain ydomain comparableRow b
facetRowWrap { row, maxPerRow } (Plot plotSpec) =
    let
        facetRow =
            FacetDetails compare toString row
    in
        Plot <|
            { plotSpec | facet = Just <| Row facetRow (Just maxPerRow) }


{-| Generate columns of small multiples, with a maximum height, using the same
    scale and axes, allowing them to be easily compared.
-}
facetColumnWrap :
    { r | column : Field data comparableColumn, maxPerColumn : Int }
    -> Plot data xdomain ydomain a b
    -> Plot data xdomain ydomain a comparableColumn
facetColumnWrap { column, maxPerColumn } (Plot plotSpec) =
    let
        facetColumn =
            FacetDetails compare toString column
    in
        Plot <|
            { plotSpec | facet = Just <| Column facetColumn (Just maxPerColumn) }


{-| Specify how the small multiple labels for each column in a row of
    small multiples should be formatted
-}
facetRowFormat : (facetRow -> String) -> Plot data xdomain ydomain facetRow facetColumn -> Plot data xdomain ydomain facetRow facetColumn
facetRowFormat format ((Plot plotSpec) as p) =
    case plotSpec.facet of
        Just (Grid facetRow facetColumn) ->
            let
                newFacetRow =
                    { facetRow | format = format }
            in
                Plot { plotSpec | facet = Just <| Grid newFacetRow facetColumn }

        Just (Row facetRow maybeLimit) ->
            let
                newFacetRow =
                    { facetRow | format = format }
            in
                Plot { plotSpec | facet = Just <| Row newFacetRow maybeLimit }

        _ ->
            p


{-| -}
facetColumnFormat : (facetColumn -> String) -> Plot data xdomain ydomain facetRow facetColumn -> Plot data xdomain ydomain facetRow facetColumn
facetColumnFormat format ((Plot plotSpec) as p) =
    case plotSpec.facet of
        Just (Grid facetRow facetColumn) ->
            let
                newFacetColumn =
                    { facetColumn | format = format }
            in
                Plot { plotSpec | facet = Just <| Grid facetRow newFacetColumn }

        Just (Column facetColumn maybeLimit) ->
            let
                newFacetColumn =
                    { facetColumn | format = format }
            in
                Plot { plotSpec | facet = Just <| Column newFacetColumn maybeLimit }

        _ ->
            p



-- Scenegraph generation -------------------------------------------------------


{-| Compile a plot with a custom `Theme`.
-}
compileWithTheme :
    Theme
    -> Plot data xdomain ydomain facetRow facetColumn
    -> List data
    -> Result (List PlotError) ( ViewBox, Scenegraph )
compileWithTheme theme (Plot plotSpec) data =
    case compileErrors plotSpec of
        [] ->
            case plotSpec.facet of
                Just (Grid facetRow facetColumn) ->
                    Ok <| compileGrid theme plotSpec facetRow facetColumn data

                Just (Row facetRow maybeLimit) ->
                    Ok <| compileRow theme plotSpec facetRow maybeLimit data

                Just (Column facetColumn maybeLimit) ->
                    Ok <| compileColumn theme plotSpec facetColumn maybeLimit data

                Nothing ->
                    Ok <|
                        compileSimple theme plotSpec data

        errs ->
            Err errs


{-| Compile a plot using the default `Theme`.
-}
compile :
    Plot data xdomain ydomain facetRow facetColumn
    -> List data
    -> Result (List PlotError) ( ViewBox, Scenegraph )
compile plot data =
    compileWithTheme Theme.default plot data



-- Generate scenegraph for different facet configurations ----------------------


compileSimple : Theme -> PlotSpec data xdomain ydomain facetRow facetColumn -> List data -> ( ViewBox, Scenegraph )
compileSimple theme plotSpec data =
    let
        xAxisOrientation =
            plotSpec.xAxis |> Maybe.map AxisInternal.verticalOrientation |> Maybe.withDefault Bottom

        yAxisOrientation =
            plotSpec.yAxis |> Maybe.map AxisInternal.horizontalOrientation |> Maybe.withDefault Left

        panelX { panelWidth, yAxisWidth, yAxisTitleWidth } currentColumn =
            case yAxisOrientation of
                Left ->
                    theme.plot.facetMargin.left + yAxisWidth + yAxisTitleWidth

                Right ->
                    theme.plot.facetMargin.left

        panelY { plotTitleHeight, xAxisTitleHeight, xAxisHeight } _ =
            case xAxisOrientation of
                Bottom ->
                    theme.plot.facetMargin.top + plotTitleHeight

                Top ->
                    theme.plot.facetMargin.top + plotTitleHeight + xAxisTitleHeight + xAxisHeight

        xAxis { panelWidth, panelHeight } xScale _ _ =
            Maybe.map (AxisInternal.scenegraphVertical theme.plot.xAxis panelWidth panelHeight xScale) plotSpec.xAxis

        yAxis { panelWidth, panelHeight } yScale _ _ =
            Maybe.map (AxisInternal.scenegraphHorizontal theme.plot.yAxis panelWidth panelHeight yScale) plotSpec.yAxis

        groupedData =
            [ [ ( Nothing, Nothing, data ) ] ]

        plotLayout =
            { nCols = 1
            , nRows = 1
            , facetTitleHeight = 0
            , totalFacetTitleHeight = 0
            , facetTitleWidth = 0
            , totalFacetTitleWidth = 0
            , xAxis = xAxis
            , xAxisOrientation = xAxisOrientation
            , yAxis = yAxis
            , yAxisOrientation = yAxisOrientation
            , facetColumnLabel = \_ _ _ _ _ _ -> Nothing
            , facetRowLabel = \_ _ _ _ _ _ -> Nothing
            , panelX = panelX
            , panelY = panelY
            }

        ( plotMeasurements, legend ) =
            calculateMeasurements theme plotSpec plotLayout
    in
        compileHelper theme plotSpec plotLayout plotMeasurements legend groupedData


compileGrid : Theme -> PlotSpec data xdomain ydomain facetRow facetColumn -> FacetDetails data facetRow -> FacetDetails data facetColumn -> List data -> ( ViewBox, Scenegraph )
compileGrid theme plotSpec facetRow facetColumn data =
    let
        {- group data and determine number of rows and columns -------------- -}
        ( nRows, nCols, groupedData ) =
            gridData facetRow facetColumn data

        facetTitleHeight =
            caclulateTitleHeight theme.plot.facetTitle

        {- when to attach an x-axis to a faceted panel -}
        xAxis { panelWidth, panelHeight } xScale =
            let
                sg =
                    Maybe.map
                        (AxisInternal.scenegraphVertical theme.plot.xAxis panelWidth panelHeight xScale)
                        plotSpec.xAxis
            in
                case xAxisOrientation of
                    Bottom ->
                        \currentRow _ ->
                            if currentRow == nRows - 1 then
                                sg
                            else
                                Nothing

                    Top ->
                        \currentRow _ ->
                            if currentRow == 0 then
                                sg
                            else
                                Nothing

        yAxis { panelWidth, panelHeight } yScale =
            let
                sg =
                    Maybe.map
                        (AxisInternal.scenegraphHorizontal theme.plot.yAxis panelWidth panelHeight yScale)
                        plotSpec.yAxis
            in
                case yAxisOrientation of
                    Left ->
                        \_ currentColumn ->
                            if currentColumn == 0 then
                                sg
                            else
                                Nothing

                    Right ->
                        \_ currentColumn ->
                            if currentColumn == nCols - 1 then
                                sg
                            else
                                Nothing

        xAxisOrientation =
            plotSpec.xAxis |> Maybe.map AxisInternal.verticalOrientation |> Maybe.withDefault Bottom

        yAxisOrientation =
            plotSpec.yAxis |> Maybe.map AxisInternal.horizontalOrientation |> Maybe.withDefault Left

        facetColumnLabel { panelWidth, panelHeight } row column x y labelText =
            case xAxisOrientation of
                Bottom ->
                    if row == 0 then
                        Just <|
                            compileHorizontalTitle theme.plot.facetTitle panelWidth x (y - facetTitleHeight) labelText
                    else
                        Nothing

                Top ->
                    if row == nRows - 1 then
                        Just <|
                            compileHorizontalTitle theme.plot.facetTitle panelWidth x (y + panelHeight) labelText
                    else
                        Nothing

        facetRowLabel { panelWidth, panelHeight } row column x y labelText =
            case yAxisOrientation of
                Left ->
                    if column == nCols - 1 then
                        Just <| compileVerticalTitle (oppositeH yAxisOrientation) theme.plot.facetTitle panelHeight (panelWidth + theme.plot.facetMargin.left) y labelText
                    else
                        Nothing

                Right ->
                    if column == 0 then
                        Just <| compileVerticalTitle (oppositeH yAxisOrientation) theme.plot.facetTitle panelHeight (x - facetTitleHeight) y labelText
                    else
                        Nothing

        panelX { panelWidth, yAxisWidth, yAxisTitleWidth } currentColumn =
            case yAxisOrientation of
                Left ->
                    (panelWidth + theme.plot.facetMargin.left + theme.plot.facetMargin.right)
                        * (toFloat currentColumn)
                        + theme.plot.facetMargin.left
                        + yAxisWidth
                        + yAxisTitleWidth

                Right ->
                    (panelWidth + theme.plot.facetMargin.left + theme.plot.facetMargin.right)
                        * (toFloat currentColumn)
                        + theme.plot.facetMargin.left
                        + facetTitleHeight

        panelY { panelHeight, plotTitleHeight, xAxisHeight, xAxisTitleHeight } currentRow =
            case xAxisOrientation of
                Bottom ->
                    (panelHeight + theme.plot.facetMargin.top + theme.plot.facetMargin.bottom)
                        * (toFloat currentRow)
                        + theme.plot.facetMargin.top
                        + facetTitleHeight
                        + plotTitleHeight

                Top ->
                    (panelHeight + theme.plot.facetMargin.top + theme.plot.facetMargin.bottom)
                        * (toFloat currentRow)
                        + theme.plot.facetMargin.top
                        + plotTitleHeight
                        + xAxisHeight
                        + xAxisTitleHeight

        plotLayout =
            { nCols = nCols
            , nRows = nRows
            , facetTitleHeight = facetTitleHeight
            , totalFacetTitleHeight = facetTitleHeight
            , facetTitleWidth = facetTitleHeight
            , totalFacetTitleWidth = facetTitleHeight
            , xAxis = xAxis
            , xAxisOrientation = xAxisOrientation
            , yAxis = yAxis
            , yAxisOrientation = yAxisOrientation
            , facetColumnLabel = facetColumnLabel
            , facetRowLabel = facetRowLabel
            , panelX = panelX
            , panelY = panelY
            }

        ( plotMeasurements, legend ) =
            calculateMeasurements theme plotSpec plotLayout
    in
        compileHelper theme plotSpec plotLayout plotMeasurements legend groupedData


compileRow : Theme -> PlotSpec data xdomain ydomain facetRow facetColumn -> FacetDetails data facetRow -> Maybe Int -> List data -> ( ViewBox, Scenegraph )
compileRow theme plotSpec facetRow maybeLimit data =
    let
        ( nRows, nCols, groupedData ) =
            rowData facetRow maybeLimit data

        facetTitleHeight =
            (caclulateTitleHeight theme.plot.facetTitle)

        totalFacetTitleHeight =
            (toFloat nRows) * facetTitleHeight

        xAxis { panelWidth, panelHeight } xScale =
            let
                sg =
                    Maybe.map
                        (AxisInternal.scenegraphVertical theme.plot.xAxis panelWidth panelHeight xScale)
                        plotSpec.xAxis

                shortestRow =
                    groupedData |> List.map List.length |> List.minimum |> Maybe.withDefault 0
            in
                case xAxisOrientation of
                    Bottom ->
                        \currentRow currentColumn ->
                            if currentRow + 1 == nRows || (currentColumn + 1 > shortestRow && currentRow + 1 == nRows - 1) then
                                sg
                            else
                                Nothing

                    Top ->
                        \currentRow currentColumn ->
                            if currentRow == 0 then
                                sg
                            else
                                Nothing

        yAxis { panelWidth, panelHeight } yScale =
            let
                sg =
                    Maybe.map
                        (AxisInternal.scenegraphHorizontal theme.plot.yAxis panelWidth panelHeight yScale)
                        plotSpec.yAxis

                shortestRow =
                    groupedData |> List.map List.length |> List.minimum |> Maybe.withDefault 0
            in
                case yAxisOrientation of
                    Left ->
                        \currentRow currentColumn ->
                            if currentColumn == 0 then
                                sg
                            else
                                Nothing

                    Right ->
                        \currentRow currentColumn ->
                            if currentColumn == nCols - 1 || (currentRow == nRows - 1 && currentColumn == shortestRow - 1) then
                                sg
                            else
                                Nothing

        xAxisOrientation =
            plotSpec.xAxis |> Maybe.map AxisInternal.verticalOrientation |> Maybe.withDefault Bottom

        yAxisOrientation =
            plotSpec.yAxis |> Maybe.map AxisInternal.horizontalOrientation |> Maybe.withDefault Left

        facetColumnLabel { panelWidth, panelHeight } row column x y labelText =
            Just <|
                case xAxisOrientation of
                    Bottom ->
                        compileHorizontalTitle theme.plot.facetTitle panelWidth x (y - facetTitleHeight) labelText

                    Top ->
                        compileHorizontalTitle theme.plot.facetTitle panelWidth x (y + panelHeight) labelText

        facetRowLabel measurements row column x y labelText =
            Nothing

        panelX { panelWidth, yAxisWidth, yAxisTitleWidth } currentColumn =
            case yAxisOrientation of
                Left ->
                    (panelWidth + theme.plot.facetMargin.left + theme.plot.facetMargin.right)
                        * (toFloat currentColumn)
                        + theme.plot.facetMargin.left
                        + yAxisWidth
                        + yAxisTitleWidth

                Right ->
                    (panelWidth + theme.plot.facetMargin.left + theme.plot.facetMargin.right)
                        * (toFloat currentColumn)
                        + theme.plot.facetMargin.left

        panelY { panelHeight, plotTitleHeight, xAxisHeight, xAxisTitleHeight } currentRow =
            case xAxisOrientation of
                Bottom ->
                    (panelHeight + facetTitleHeight + theme.plot.facetMargin.top + theme.plot.facetMargin.bottom)
                        * (toFloat currentRow)
                        + plotTitleHeight
                        + facetTitleHeight
                        + theme.plot.facetMargin.top

                Top ->
                    (panelHeight + facetTitleHeight + theme.plot.facetMargin.top + theme.plot.facetMargin.bottom)
                        * (toFloat currentRow)
                        + plotTitleHeight
                        + xAxisHeight
                        + xAxisTitleHeight

        plotLayout =
            { nCols = nCols
            , nRows = nRows
            , facetTitleHeight = facetTitleHeight
            , totalFacetTitleHeight = totalFacetTitleHeight
            , facetTitleWidth = 0
            , totalFacetTitleWidth = 0
            , xAxis = xAxis
            , xAxisOrientation = xAxisOrientation
            , yAxis = yAxis
            , yAxisOrientation = yAxisOrientation
            , facetColumnLabel = facetColumnLabel
            , facetRowLabel = facetRowLabel
            , panelX = panelX
            , panelY = panelY
            }

        ( plotMeasurements, legend ) =
            calculateMeasurements theme plotSpec plotLayout
    in
        compileHelper theme plotSpec plotLayout plotMeasurements legend groupedData


compileColumn : Theme -> PlotSpec data xdomain ydomain facetRow facetColumn -> FacetDetails data facetColumn -> Maybe Int -> List data -> ( ViewBox, Scenegraph )
compileColumn theme plotSpec facetColumn maybeLimit data =
    let
        ( nRows, nCols, groupedData ) =
            columnData facetColumn maybeLimit data

        facetTitleWidth =
            (caclulateTitleHeight theme.plot.facetTitle)

        totalFacetTitleWidth =
            (toFloat nCols) * facetTitleWidth

        xAxis { panelWidth, panelHeight } xScale =
            let
                sg =
                    Maybe.map
                        (AxisInternal.scenegraphVertical theme.plot.xAxis panelWidth panelHeight xScale)
                        plotSpec.xAxis

                shortestRow =
                    groupedData |> List.map List.length |> List.minimum |> Maybe.withDefault 0
            in
                case xAxisOrientation of
                    Bottom ->
                        \currentRow currentColumn ->
                            if currentRow + 1 == nRows || (currentColumn + 1 > shortestRow && currentRow + 1 == nRows - 1) then
                                sg
                            else
                                Nothing

                    Top ->
                        \currentRow currentColumn ->
                            if currentRow == 0 then
                                sg
                            else
                                Nothing

        yAxis { panelWidth, panelHeight } yScale =
            let
                sg =
                    Maybe.map
                        (AxisInternal.scenegraphHorizontal theme.plot.yAxis panelWidth panelHeight yScale)
                        plotSpec.yAxis

                shortestRow =
                    groupedData |> List.map List.length |> List.minimum |> Maybe.withDefault 0
            in
                case yAxisOrientation of
                    Left ->
                        \currentRow currentColumn ->
                            if currentColumn == 0 then
                                sg
                            else
                                Nothing

                    Right ->
                        \currentRow currentColumn ->
                            if currentColumn == nCols - 1 || (currentRow == nRows - 1 && currentColumn == shortestRow - 1) then
                                sg
                            else
                                Nothing

        xAxisOrientation =
            plotSpec.xAxis |> Maybe.map AxisInternal.verticalOrientation |> Maybe.withDefault Bottom

        yAxisOrientation =
            plotSpec.yAxis |> Maybe.map AxisInternal.horizontalOrientation |> Maybe.withDefault Left

        facetRowLabel measurements row column x y labelText =
            Nothing

        facetColumnLabel { panelWidth, panelHeight } row column x y labelText =
            Just <|
                case yAxisOrientation of
                    Left ->
                        compileVerticalTitle Right theme.plot.facetTitle panelHeight (x + panelWidth) y labelText

                    Right ->
                        compileVerticalTitle Left theme.plot.facetTitle panelHeight (x - facetTitleWidth) y labelText

        panelX { panelWidth, yAxisWidth, yAxisTitleWidth } currentColumn =
            case yAxisOrientation of
                Left ->
                    (panelWidth + facetTitleWidth + theme.plot.facetMargin.left + theme.plot.facetMargin.right)
                        * (toFloat currentColumn)
                        + theme.plot.facetMargin.left
                        + yAxisWidth
                        + yAxisTitleWidth

                Right ->
                    (panelWidth + theme.plot.facetMargin.left + theme.plot.facetMargin.right)
                        * (toFloat currentColumn)
                        + theme.plot.facetMargin.left
                        + facetTitleWidth

        panelY { panelHeight, plotTitleHeight, xAxisHeight, xAxisTitleHeight } currentRow =
            case xAxisOrientation of
                Bottom ->
                    (panelHeight + theme.plot.facetMargin.top + theme.plot.facetMargin.bottom)
                        * (toFloat currentRow)
                        + plotTitleHeight
                        + theme.plot.facetMargin.top

                Top ->
                    (panelHeight + theme.plot.facetMargin.top + theme.plot.facetMargin.bottom)
                        * (toFloat currentRow)
                        + plotTitleHeight
                        + xAxisHeight
                        + xAxisTitleHeight

        plotLayout =
            { nCols = nCols
            , nRows = nRows
            , facetTitleHeight = 0
            , totalFacetTitleHeight = 0
            , facetTitleWidth = facetTitleWidth
            , totalFacetTitleWidth = totalFacetTitleWidth
            , xAxis = xAxis
            , xAxisOrientation = xAxisOrientation
            , yAxis = yAxis
            , yAxisOrientation = yAxisOrientation
            , facetColumnLabel = facetColumnLabel
            , facetRowLabel = facetRowLabel
            , panelX = panelX
            , panelY = panelY
            }

        ( plotMeasurements, legend ) =
            calculateMeasurements theme plotSpec plotLayout
    in
        compileHelper theme plotSpec plotLayout plotMeasurements legend groupedData


compileHelper : Theme -> PlotSpec data xdomain ydomain facetRow facetColumn -> PlotLayout xdomain ydomain -> PlotMeasurements -> Maybe Scenegraph -> List (List ( Maybe String, Maybe String, List data )) -> ( ViewBox, Scenegraph )
compileHelper theme plotSpec plotLayout plotMeasurements legend groupedData =
    let
        xScale =
            mkXScale plotSpec groupedData plotMeasurements.panelWidth

        yScale =
            mkYScale plotSpec groupedData plotMeasurements.panelHeight

        plotTitle =
            plotSpec.title
                |> Maybe.map (compileHorizontalTitle theme.title plotMeasurements.allPanelWidth (plotMeasurements.yAxisTitleWidth + plotMeasurements.yAxisWidth) 0)

        panelBackground =
            compilePanelBackground theme plotMeasurements.panelWidth plotMeasurements.panelHeight

        xAxisTitle =
            plotSpec.xAxis
                |> Maybe.andThen AxisInternal.getTitle
                |> Maybe.map
                    (\title ->
                        let
                            xpos =
                                (plotMeasurements.yAxisTitleWidth + plotMeasurements.yAxisWidth)

                            ypos =
                                case plotLayout.xAxisOrientation of
                                    Bottom ->
                                        plotMeasurements.plotTitleHeight + plotMeasurements.xAxisHeight + plotMeasurements.allPanelHeight

                                    Top ->
                                        plotMeasurements.plotTitleHeight
                        in
                            compileHorizontalTitle theme.plot.xAxis.title plotMeasurements.allPanelWidth xpos ypos title
                    )

        yAxisTitle =
            plotSpec.yAxis
                |> Maybe.andThen AxisInternal.getTitle
                |> Maybe.map
                    (\title ->
                        let
                            ypos =
                                plotMeasurements.plotTitleHeight

                            xpos =
                                case plotLayout.yAxisOrientation of
                                    Left ->
                                        0

                                    Right ->
                                        plotMeasurements.allPanelWidth + plotMeasurements.yAxisWidth
                        in
                            compileVerticalTitle plotLayout.yAxisOrientation theme.plot.yAxis.title plotMeasurements.allPanelHeight xpos ypos title
                    )

        panels =
            compilePanels
                theme
                plotSpec
                plotLayout
                plotMeasurements
                panelBackground
                xScale
                yScale
                groupedData

        elems =
            List.concat
                [ plotTitle |> Maybe.toList
                , legend |> Maybe.toList
                , xAxisTitle |> Maybe.toList
                , yAxisTitle |> Maybe.toList
                , panels
                ]

        groupInfo =
            Mark.group theme.margin.left plotMeasurements.effectiveWidth theme.margin.top plotMeasurements.effectiveHeight False
    in
        ( ViewBox 0 0 plotMeasurements.totalWidth plotMeasurements.totalHeight
        , Scenegraph.Group [ ( groupInfo, elems ) ]
        )



-- Plot compilation errors -----------------------------------------------------


{-| Check the plot has required axes
-}
compileErrors : PlotSpec data xdomain ydomain facetRow facetColumn -> List PlotError
compileErrors { xAxis, yAxis, layers } =
    List.filterMap identity
        [ if Maybe.isNothing xAxis && requiresXAxis layers then
            Just NoXAxis
          else
            Nothing
        , if Maybe.isNothing yAxis && requiresYAxis layers then
            Just NoYAxis
          else
            Nothing
        ]


requiresXAxis : List (Encoding data xdomain ydomain) -> Bool
requiresXAxis layers =
    layers
        |> List.filterMap (Maybe.map not << Encoding.hasConstantXPosition)
        |> List.any identity


requiresYAxis : List (Encoding data xdomain ydomain) -> Bool
requiresYAxis layers =
    layers
        |> List.filterMap (Maybe.map not << Encoding.hasConstantYPosition)
        |> List.any identity



-- Scenegraph Helpers ----------------------------------------------------------


compilePanels : Theme -> PlotSpec data xdomain ydomain facetRow facetColumn -> PlotLayout xdomain ydomain -> PlotMeasurements -> Scenegraph -> Scale.Scale xdomain Float -> Scale.Scale ydomain Float -> List (List ( Maybe String, Maybe String, List data )) -> List Scenegraph
compilePanels theme plotSpec plotLayout plotMeasurements panelBackground xScale yScale groupedData =
    groupedData
        |> List.indexedMap
            (\currentRow ->
                List.indexedMap
                    (\currentColumn ( rowLabel, columnLabel, data ) ->
                        let
                            xpos =
                                plotLayout.panelX plotMeasurements currentColumn

                            ypos =
                                plotLayout.panelY plotMeasurements currentRow

                            markGroupInfo =
                                Mark.group 0 plotMeasurements.panelWidth 0 plotMeasurements.panelHeight True

                            clippedMarks =
                                Scenegraph.Group
                                    [ ( markGroupInfo
                                      , plotSpec.layers
                                            |> List.reverse
                                            |> List.map (Encoding.scenegraph theme.mark xScale yScale data)
                                      )
                                    ]

                            elems =
                                List.filterMap identity
                                    [ Just panelBackground
                                    , plotLayout.yAxis plotMeasurements yScale currentRow currentColumn
                                    , plotLayout.xAxis plotMeasurements xScale currentRow currentColumn
                                    , columnLabel
                                        |> Maybe.andThen (plotLayout.facetColumnLabel plotMeasurements currentRow currentColumn 0 0)
                                    , rowLabel
                                        |> Maybe.andThen (plotLayout.facetRowLabel plotMeasurements currentRow currentColumn 0 0)
                                    , Just clippedMarks
                                    ]

                            groupInfo =
                                Mark.group xpos 0 ypos 0 False
                        in
                            Scenegraph.Group [ ( groupInfo, elems ) ]
                    )
            )
        |> List.concat


compileHorizontalTitle : Theme.Title -> Float -> Float -> Float -> String -> Scenegraph
compileHorizontalTitle titleTheme width xpos ypos titleText =
    let
        titleMargin =
            titleTheme.margin

        titlePadding =
            titleTheme.padding

        titleLabel =
            titleTheme.label

        titleFont =
            titleTheme.label.font

        rectX =
            titleMargin.left

        rectX2 =
            width - titleMargin.right

        rectY =
            titleMargin.top

        rectY2 =
            titleMargin.top + titlePadding.top + titlePadding.bottom + titleFont.fontSize

        height =
            rectY2 + titleMargin.bottom

        rect =
            Mark.rect rectX rectX2 rectY rectY2
                |> Mark.fill titleTheme.fill
                |> Mark.stroke titleTheme.stroke
                |> List.singleton
                |> Scenegraph.Rect

        text =
            Mark.text (width / 2) (height / 2) titleText
                |> Mark.stroke titleLabel.stroke
                |> Mark.fill titleLabel.fill
                |> Mark.font titleLabel.font
                |> Mark.align Mark.Center
                |> List.singleton
                |> Scenegraph.Text

        groupInfo =
            Mark.group xpos width ypos height False
    in
        Scenegraph.Group [ ( groupInfo, [ rect, text ] ) ]


compileVerticalTitle : Horizontal -> Theme.Title -> Float -> Float -> Float -> String -> Scenegraph
compileVerticalTitle leftOrRight titleTheme panelHeight x y labelText =
    let
        height =
            panelHeight

        padding =
            titleTheme.padding

        margin =
            titleTheme.margin

        fill =
            titleTheme.fill

        stroke =
            titleTheme.stroke

        font =
            titleTheme.label.font

        textFill =
            titleTheme.label.fill

        textStroke =
            titleTheme.label.stroke

        rectX =
            margin.left

        rectX2 =
            margin.left + padding.top + font.fontSize + padding.bottom

        rectY =
            margin.top

        rectY2 =
            margin.top + height - margin.right

        width =
            rectX2 + margin.right

        rect =
            Mark.rect rectX rectX2 rectY rectY2
                |> Mark.fill fill
                |> Mark.stroke stroke
                |> List.singleton
                |> Scenegraph.Rect

        theta =
            case leftOrRight of
                Left ->
                    -pi / 2

                _ ->
                    pi / 2

        text =
            Mark.text (width / 2) (height / 2) labelText
                |> Mark.font font
                |> Mark.fill textFill
                |> Mark.stroke textStroke
                |> Mark.angle theta
                |> List.singleton
                |> Scenegraph.Text

        group =
            Mark.group x width y height False
    in
        Scenegraph.Group [ ( group, [ rect, text ] ) ]


compilePanelBackground : Theme -> Float -> Float -> Scenegraph
compilePanelBackground theme panelWidth panelHeight =
    let
        rect =
            Mark.rect 0.0 panelWidth 0.0 panelHeight
                |> Mark.stroke theme.plot.stroke
                |> Mark.fill theme.plot.fill
    in
        Scenegraph.Rect [ rect ]



-- compileAxis : AxisTheme.Axis -> Maybe (Axis domain) -> Float -> Float -> Scale.Scale domain Float -> Int -> Maybe Int -> Int -> Maybe Scenegraph
-- compileAxis axisTheme axisSpec panelWidth panelHeight scale n maybeLimit =
--     case axisSpec of
--         Nothing ->
--             \_ -> Nothing
--
--         Just axis ->
--             let
--                 sg =
--                     Axis.scenegraph axisTheme panelWidth panelHeight scale axis
--             in
--                 case Axis.orientation axis of
--                     Horizontal Left ->
--                         \i ->
--                             if i == 0 then
--                                 Just sg
--                             else
--                                 Nothing
--
--                     Horizontal Right ->
--                         \i ->
--                             if i == n - 1 then
--                                 Just sg
--                             else
--                                 Nothing
--
--                     Vertical Top ->
--                         \i ->
--                             if i == 0 then
--                                 Just sg
--                             else
--                                 Nothing
--
--                     Vertical Bottom ->
--                         \i ->
--                             if i == n - 1 then
--                                 Just sg
--                             else
--                                 Nothing
--


{-| Extract and arrange all legends and calculate total width and height of
    the legend group
-}
compileLegends : Theme -> PlotSpec data xdomain ydomain facetRow facetColumn -> Float -> Float -> ( Maybe Scenegraph, Float, Float )
compileLegends theme plotSpec effectiveWidth effectiveHeight =
    let
        ( legends, ( legendWidthBase, legendHeightBase ) ) =
            plotSpec.layers
                |> List.concatMap (Encoding.legends (Maybe.withDefault 4 plotSpec.legendItems) >> Legend.scenegraph theme.legend)
                |> arrangeLegendsVertical theme.legend.margin effectiveHeight

        legendWidth =
            if List.isEmpty legends then
                0
            else
                legendWidthBase + theme.legendGroupMargin.left + theme.legendGroupMargin.right

        legendHeight =
            if List.isEmpty legends then
                0
            else
                legendHeightBase + theme.legendGroupMargin.top + theme.legendGroupMargin.bottom

        legend =
            case legends of
                [] ->
                    Nothing

                _ ->
                    let
                        legendX =
                            effectiveWidth - legendWidth + theme.legendGroupMargin.left

                        legendY =
                            theme.legendGroupMargin.top
                    in
                        Just <| Scenegraph.Group [ ( Mark.group legendX legendWidth legendY legendHeight False, legends ) ]
    in
        ( legend, legendWidth, legendHeight )


{-| Arrange legends, stacking vertically


    Each item is the legend scenegraph along with it's required width and
    height.

    The state is the list positioned legend scenegraphs along with the total
    width and height of the combined scenegraphs

    When adding the additional legend to teh bottom of the stack would overflow,
    we start from the top again and stack to the right of the current total width
-}
arrangeLegendsVertical : Theme.Margin -> Float -> List ( Scenegraph, ( Float, Float ) ) -> ( List Scenegraph, ( Float, Float ) )
arrangeLegendsVertical margin maxHeight legends =
    let
        ( sg, ( width, height, _, _ ) ) =
            List.foldl
                (\( sg, ( w, h ) ) ( accu, ( ttlWidth, ttlHeight, x, y ) ) ->
                    if y + h > maxHeight then
                        let
                            x2 =
                                x + ttlWidth + margin.right

                            y2 =
                                0

                            grp =
                                Scenegraph.Group [ ( Mark.group x2 w y2 h False, [ sg ] ) ]

                            newX =
                                x2

                            newY =
                                y2 + h + margin.bottom

                            newTtlWidth =
                                (x2 + w)

                            newTtlHeight =
                                max ttlHeight newY

                            newMeasure =
                                ( newTtlWidth, newTtlHeight, newX, newY )
                        in
                            ( grp :: accu, newMeasure )
                    else
                        let
                            grp =
                                Scenegraph.Group [ ( Mark.group x w y h False, [ sg ] ) ]

                            newX =
                                x

                            newY =
                                y + h + margin.bottom

                            newTtlWidth =
                                max ttlWidth (x + w)

                            newTtlHeight =
                                max ttlHeight newY

                            newMeasure =
                                ( newTtlWidth, newTtlHeight, newX, newY )
                        in
                            ( grp :: accu, newMeasure )
                )
                ( [], ( 0, 0, 0, 0 ) )
                legends
    in
        ( sg, ( width, height ) )



-- Measure Scenegraph elemensts ------------------------------------------------


calculateMeasurements : Theme -> PlotSpec data xdomain ydomain facetRow facetColumn -> PlotLayout xdomain ydomain -> ( PlotMeasurements, Maybe Scenegraph )
calculateMeasurements theme plotSpec plotLayout =
    let
        totalWidth =
            Maybe.withDefault theme.plot.width plotSpec.width

        effectiveWidth =
            totalWidth - theme.margin.left - theme.margin.right

        totalHeight =
            Maybe.withDefault theme.plot.height plotSpec.height

        effectiveHeight =
            totalHeight - theme.margin.top - theme.margin.bottom

        ( legend, legendWidth, legendHeight ) =
            compileLegends theme plotSpec effectiveWidth effectiveHeight

        xAxisHeight =
            Maybe.map (AxisInternal.calculateXAxisHeight theme.plot.xAxis) plotSpec.xAxis |> Maybe.withDefault 0

        xAxisTitleHeight =
            plotSpec.xAxis
                |> Maybe.andThen AxisInternal.getTitle
                |> Maybe.maybe 0 (\_ -> caclulateTitleHeight theme.plot.xAxis.title)

        yAxisWidth =
            Maybe.map (AxisInternal.calculateYAxisWidth theme.plot.yAxis) plotSpec.yAxis |> Maybe.withDefault 0

        yAxisTitleWidth =
            plotSpec.yAxis
                |> Maybe.andThen AxisInternal.getTitle
                |> Maybe.maybe 0 (\_ -> caclulateTitleHeight theme.plot.yAxis.title)

        plotTitleHeight =
            plotSpec.title
                |> Maybe.maybe 0 (\_ -> caclulateTitleHeight theme.title)

        availableWidth =
            effectiveWidth - legendWidth

        availableHeight =
            effectiveHeight - plotTitleHeight

        allPanelWidth =
            availableWidth - yAxisWidth - yAxisTitleWidth

        allPanelHeight =
            availableHeight - xAxisHeight - xAxisTitleHeight

        panelWidth =
            let
                totalMargin =
                    (theme.plot.facetMargin.left + theme.plot.facetMargin.right) * (toFloat <| plotLayout.nCols - 1)
            in
                (allPanelWidth - plotLayout.totalFacetTitleWidth - totalMargin) / (toFloat plotLayout.nCols)

        panelHeight =
            let
                totalMargin =
                    (theme.plot.facetMargin.top + theme.plot.facetMargin.bottom) * (toFloat <| plotLayout.nRows - 1)
            in
                (allPanelHeight - plotLayout.totalFacetTitleHeight - totalMargin) / (toFloat plotLayout.nRows)
    in
        ( { totalWidth = totalWidth
          , totalHeight = totalHeight
          , effectiveWidth = effectiveWidth
          , effectiveHeight = effectiveHeight
          , legendWidth = legendWidth
          , legendHeight = legendHeight
          , availableWidth = availableWidth
          , availableHeight = availableHeight
          , xAxisHeight = xAxisHeight
          , xAxisTitleHeight = xAxisTitleHeight
          , yAxisWidth = yAxisWidth
          , yAxisTitleWidth = yAxisTitleWidth
          , plotTitleHeight = plotTitleHeight
          , allPanelWidth = allPanelWidth
          , allPanelHeight = allPanelHeight
          , panelWidth = panelWidth
          , panelHeight = panelHeight
          }
        , legend
        )


caclulateTitleHeight : Theme.Title -> Float
caclulateTitleHeight title =
    let
        padding =
            title.padding

        margin =
            title.margin

        fontSize =
            title.label.font.fontSize
    in
        padding.top + padding.bottom + margin.top + margin.bottom + fontSize



-- Axes / Domain extent --------------------------------------------------------


{-| Create a scale for use with the x-axis determining the extent of the
    domain if none was explicitly set in the `PlotSpec`
-}
mkXScale : PlotSpec data xdomain ydomain facetRow facetColumn -> List (List ( Maybe String, Maybe String, List data )) -> Float -> Scale.Scale xdomain Float
mkXScale plotSpec groupedData panelWidth =
    case plotSpec.xAxis of
        Nothing ->
            Scale.constant 0

        Just xAxis ->
            Maybe.withDefault (Scale.constant 0) <|
                if AxisInternal.hasDomain xAxis then
                    if AxisInternal.isContinuous xAxis then
                        AxisInternal.getContinuousScale Nothing ( 0, panelWidth ) xAxis
                    else
                        AxisInternal.getOrdinalScale Nothing ( 0, panelWidth ) xAxis
                else
                    let
                        data =
                            List.concatMap (List.concatMap (\( _, _, xs ) -> xs)) groupedData
                    in
                        if AxisInternal.isContinuous xAxis then
                            AxisInternal.getContinuousScale (extentXContinuous data plotSpec.layers) ( 0, panelWidth ) xAxis
                        else
                            AxisInternal.getOrdinalScale (extentXDiscrete data plotSpec.layers) ( 0, panelWidth ) xAxis


{-| Create a scale for use with the y-axis determining the extent of the
    domain if none was explicitly set in the `PlotSpec`
-}
mkYScale : PlotSpec data xdomain ydomain facetRow facetColumn -> List (List ( Maybe String, Maybe String, List data )) -> Float -> Scale.Scale ydomain Float
mkYScale plotSpec groupedData panelHeight =
    let
        data =
            List.concatMap (List.concatMap (\( _, _, xs ) -> xs)) groupedData
    in
        case plotSpec.yAxis of
            Nothing ->
                Scale.constant 0

            Just yAxis ->
                Maybe.withDefault (Scale.constant 0) <|
                    if AxisInternal.hasDomain yAxis then
                        if AxisInternal.isContinuous yAxis then
                            AxisInternal.getContinuousScale Nothing ( panelHeight, 0 ) yAxis
                        else
                            AxisInternal.getOrdinalScale Nothing ( panelHeight, 0 ) yAxis
                    else if AxisInternal.isContinuous yAxis then
                        AxisInternal.getContinuousScale (extentYContinuous data plotSpec.layers) ( panelHeight, 0 ) yAxis
                    else
                        AxisInternal.getOrdinalScale (extentYDiscrete data plotSpec.layers) ( panelHeight, 0 ) yAxis


extentXContinuous : List data -> List (Encoding data xdomain ydomain) -> Maybe ( xdomain, xdomain )
extentXContinuous data layers =
    List.foldl (Encoding.extentXContinuous data) ( Nothing, Nothing ) layers
        |> \( x, y ) -> Maybe.map2 (,) x y


extentYContinuous : List data -> List (Encoding data xdomain ydomain) -> Maybe ( ydomain, ydomain )
extentYContinuous data layers =
    List.foldl (Encoding.extentYContinuous data) ( Nothing, Nothing ) layers
        |> \( x, y ) -> Maybe.map2 (,) x y


extentXDiscrete : List data -> List (Encoding data xdomain ydomain) -> Maybe (List xdomain)
extentXDiscrete data layers =
    case List.foldl (Encoding.extentXDiscrete data) [] layers of
        [] ->
            Nothing

        extent ->
            Just extent


extentYDiscrete : List data -> List (Encoding data xdomain ydomain) -> Maybe (List ydomain)
extentYDiscrete data layers =
    case List.foldl (Encoding.extentYDiscrete data) [] layers of
        [] ->
            Nothing

        extent ->
            Just extent



-- Data manipulation -----------------------------------------------------------


{-| Apply row and column facet fields to create a grid of data with
    corresponding labels
-}
gridData :
    FacetDetails data facetRow
    -> FacetDetails data facetColumn
    -> List data
    -> ( Int, Int, List (List ( Maybe String, Maybe String, List data )) )
gridData facetRow facetColumn data =
    let
        extractRow =
            Field.extract facetRow.field

        orderByRow =
            compareMaybe extractRow facetRow.compareWith

        extractColumn =
            Field.extract facetColumn.field

        orderByColumn =
            compareMaybe extractColumn facetColumn.compareWith

        allRow =
            data
                |> List.filterMap extractRow
                |> List.unique facetRow.compareWith

        nRow =
            List.length allRow

        allColumn =
            data
                |> List.filterMap extractColumn
                |> List.unique facetColumn.compareWith

        nColumn =
            List.length allColumn

        groupedData =
            data
                |> List.groupBy orderByRow extractRow
                |> List.filterMap
                    (\( lbl, data ) ->
                        Maybe.map (\lbl -> ( lbl, data )) lbl
                    )
                |> augment allRow
                |> List.map
                    (\( rowLabel, data ) ->
                        data
                            |> List.groupBy orderByColumn extractColumn
                            |> List.filterMap
                                (\( lbl, data ) ->
                                    Maybe.map (\lbl -> ( lbl, data )) lbl
                                )
                            |> augment allColumn
                            |> List.map
                                (\( columnLabel, data ) ->
                                    ( Just <| facetRow.format rowLabel
                                    , Just <| facetColumn.format columnLabel
                                    , data
                                    )
                                )
                    )
    in
        ( nRow, nColumn, groupedData )


{-| Apply row facet field and optional row length limit to create a grid of data
    with corresponding labels.
-}
rowData :
    FacetDetails data facetRow
    -> Maybe Int
    -> List data
    -> ( Int, Int, List (List ( Maybe String, Maybe String, List data )) )
rowData facetRow maybeLimit data =
    let
        extractRow =
            Field.extract facetRow.field

        orderByRow =
            compareMaybe extractRow facetRow.compareWith

        allRow =
            data
                |> List.filterMap extractRow
                |> List.unique facetRow.compareWith

        nWide =
            List.length allRow

        nHigh =
            Maybe.maybe 1
                (\limit -> ceiling <| (toFloat nWide) / (toFloat limit))
                maybeLimit

        groupedData =
            data
                |> List.groupBy orderByRow extractRow
                |> List.filterMap
                    (\( lbl, data ) ->
                        Maybe.map (\lbl -> ( lbl, data )) lbl
                    )
                |> augment allRow
                |> List.chop maybeLimit
                |> List.map (List.map (\( lbl, data ) -> ( Nothing, Just <| facetRow.format lbl, data )))
    in
        ( nHigh, Maybe.withDefault nWide maybeLimit, groupedData )


{-| Apply column facet field and optional row length limit to create a grid
    of data with corresponding labels.
-}
columnData :
    FacetDetails data facetColumn
    -> Maybe Int
    -> List data
    -> ( Int, Int, List (List ( Maybe String, Maybe String, List data )) )
columnData facetColumn maybeLimit data =
    rowData facetColumn maybeLimit data
        |> (\( nCol, nRow, data ) -> ( nRow, nCol, List.transpose data ))



-- Helpers ---------------------------------------------------------------------


{-| NB this requires the inputs to be sorted by their labels
-}
augment : List a -> List ( a, List b ) -> List ( a, List b )
augment allLabels data =
    augmentHelper [] allLabels data


augmentHelper : List ( a, List b ) -> List a -> List ( a, List b ) -> List ( a, List b )
augmentHelper accu lbls data =
    case ( lbls, data ) of
        ( nextLabel :: restLbls, ( lbl, nextData ) :: restData ) ->
            if lbl == nextLabel then
                augmentHelper (( lbl, nextData ) :: accu) restLbls restData
            else
                augmentHelper (( nextLabel, [] ) :: accu) restLbls data

        ( nextLbls :: restLbls, _ ) ->
            augmentHelper (( nextLbls, [] ) :: accu) restLbls []

        _ ->
            List.reverse accu


oppositeH : Horizontal -> Horizontal
oppositeH hOrientation =
    case hOrientation of
        Left ->
            Right

        Right ->
            Left
