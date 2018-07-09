module Main exposing (main)

import Html exposing (program, Html)
import Html.Attributes as A
import Http
import Task
import Color exposing (Color)
import Data.Cars as Cars exposing (Car)
import Data.Birdstrikes as Birdstrikes exposing (Birdstrike)
import Facet.Axis as Axis
import Facet.Channel as Channel
import Facet.Encoding as Encoding exposing (Encoding)
import Facet.Field as Field
import Facet.Plot as Plot exposing (Plot)
import Facet.Render.Svg exposing (render)
import Facet.Scale as Scale
import Facet.Scenegraph exposing (Scenegraph, ViewBox)
import Facet.Scenegraph.Mark exposing (Behaviour(..))
import Facet.Scenegraph.Interpolate exposing (Interpolate(..))


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Task.attempt LoadData loadData )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


loadData : Task.Task Http.Error ( List Car, List Birdstrike )
loadData =
    Cars.cars ()
        |> Task.andThen
            (\cars ->
                Birdstrikes.birdstrikes ()
                    |> Task.map ((,) cars)
            )



-- View ------------------------------------------------------------------------


view : Model -> Html msg
view model =
    let
        content =
            case ( model.state, model.error ) of
                ( Loading, _ ) ->
                    [ Html.h1 [] [ Html.text "Loading" ] ]

                ( Loaded, Just err ) ->
                    [ Html.text err ]

                _ ->
                    let
                        carPlots =
                            model.carScenegraphs
                                |> List.map
                                    (\( maybePlot, info ) ->
                                        Maybe.map
                                            (\plot -> Html.div [] [ Html.hr [] [], Html.p [] [ Html.text info ], render Nothing Nothing plot ])
                                            maybePlot
                                    )
                                |> List.filterMap identity

                        birdStrikePlots =
                            model.birdstrikesScenegraphs
                                |> List.map
                                    (\( maybePlot, info ) ->
                                        Maybe.map
                                            (\plot -> Html.div [] [ Html.hr [] [], Html.p [] [ Html.text info ], render Nothing Nothing plot ])
                                            maybePlot
                                    )
                                |> List.filterMap identity

                        otherPlots =
                            model.otherScenegraphs
                                |> List.map
                                    (\( maybePlot, info ) ->
                                        Maybe.map
                                            (\plot -> Html.div [] [ Html.hr [] [], Html.p [] [ Html.text info ], render Nothing Nothing plot ])
                                            maybePlot
                                    )
                                |> List.filterMap identity
                    in
                        [ Html.h1 [] [ Html.text "Car data" ]
                        , Html.div [ A.style plotStyles ] carPlots
                        , Html.h1 [] [ Html.text "Birdstrike data" ]
                        , Html.div [ A.style plotStyles ] birdStrikePlots
                        , Html.h1 [] [ Html.text "Other examples" ]
                        , Html.div [ A.style plotStyles ] otherPlots
                        ]
    in
        Html.div
            [ A.style wrapperStyles ]
            [ Html.div
                [ A.style contentStyles ]
                content
            ]


wrapperStyles : List ( String, String )
wrapperStyles =
    [ ( "display", "flex" )
    , ( "flex-direction", "row" )
    , ( "justify-content", "center" )
    ]


contentStyles : List ( String, String )
contentStyles =
    [ ( "flex-basis", "768px" )
    , ( "display", "flex" )
    , ( "flex-direction", "column" )
    ]


plotStyles : List ( String, String )
plotStyles =
    [ ( "border", "1 0 1 0 solid #333" ), ( "padding", "8 0 0 8" ) ]



--------------------------------------------------------------------------------


type alias Model =
    { state : State
    , cars : List Car
    , birdstrikes : List Birdstrike
    , error : Maybe String
    , carScenegraphs : List ( Maybe ( ViewBox, Scenegraph ), String )
    , birdstrikesScenegraphs : List ( Maybe ( ViewBox, Scenegraph ), String )
    , otherScenegraphs : List ( Maybe ( ViewBox, Scenegraph ), String )
    }


initialModel : Model
initialModel =
    Model Loading [] [] Nothing [] [] []


type State
    = Loading
    | Loaded


type Msg
    = LoadData (Result Http.Error ( List Car, List Birdstrike ))


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        LoadData (Err err) ->
            ( { model | state = Loaded, error = Just <| toString err }, Cmd.none )

        LoadData (Ok ( cars, birdstrikes )) ->
            let
                carScenegraphs =
                    [ ( compileWith cars scatterPlot, "A simple scatter plot" )
                    , ( compileWith cars scatterPlotReversed, "The same plot with the axes oriented on the top and right. Note that when faceting the labels will orient themselves to avoid the axes wherever you choose to place them." )
                    , ( compileWith cars scatterPlotFacetted, "The same plot faceted by origin and number of cylinders." )
                    , ( compileWith cars scatterPlot2, "A scatter plot with acceleration encoded as the size of the mark." )
                    , ( compileWith cars barPlot, "A bar plot using an `AggregateField` to find the average miles per gallon." )
                    , ( compileWith cars barPlotRow, "The same bar plot faceted by number of cylinders as a row." )
                    , ( compileWith cars barPlotColumn, "The same bar plot again now faceted by number of cylinders as a column." )
                    ]

                birdstrikesScenegraphs =
                    [ ( compileWith birdstrikes birdBarPlot, "A bar plot with a square root scale to show skewed data." ) ]

                otherScenegraphs =
                    [ ( compileWith vectorFieldData vectorFieldPlot, "A symbol plot using the built-in arrow shape to represent a vector field. Note that all legends are generated from the plot declaration automatically." )
                    , ( compileWith trailData trailPlot, "A trail plot doing nothing particularly interesting." )
                    , ( compileWith lineData linePlot, "A line plot showing the different interpolation methods applied to the sin function." )
                    , ( compileWith areaData areaBeginNew, "An area plot showing the `BeginNew` behaviour when missing data (encoded as `Maybe`) is encountered ." )
                    , ( compileWith areaData areaSkipMissing, "The same area plot showing the `SkipMissing` behaviour when missing data (encoded as `Maybe`) is encountered ." )
                    , ( compileWith boolData boolPlot, "A bar plot aggregating values over non-comparable fields using a custom band scale." )
                    , ( compileWith discreteData boxPlot, "A symbol plot with discrete positions and the size encoded as the sum of values at each coordinate." )
                    ]
            in
                ( { model
                    | state = Loaded
                    , cars = cars
                    , birdstrikes = birdstrikes
                    , error = Nothing
                    , carScenegraphs = carScenegraphs
                    , birdstrikesScenegraphs = birdstrikesScenegraphs
                    , otherScenegraphs = otherScenegraphs
                  }
                , Cmd.none
                )


compileWith :
    List data
    -> Plot data xdomain ydomain facetRow facetColumn
    -> Maybe ( ViewBox, Scenegraph )
compileWith data sg =
    Plot.compile sg data |> Result.toMaybe



-- PLOTS -----------------------------------------------------------------------
-- Car data plots --------------------------------------------------------------


{-| Scatter plot of acceleration against displacement
-}
scatterPlot : Plot Car Float Float facetRow facetColumn
scatterPlot =
    Plot.plot (Just "Acceleration vs. Displacement")
        |> Plot.xAxis (Axis.linearX (Just "acceleration") |> Axis.ticks 5 |> Axis.continuousDomain ( 0, 30 ) |> Axis.labelAngle angle)
        |> Plot.yAxis (Axis.linearY (Just "displacement") |> Axis.ticks 5 |> Axis.continuousDomain ( 0, 50 ) |> Axis.labelAngle angle)
        |> Plot.width 768
        |> Plot.height 600
        |> Plot.layer carPoint


scatterPlotReversed : Plot Car Float Float facetRow facetColumn
scatterPlotReversed =
    Plot.plot (Just "Acceleration vs. Displacement")
        |> Plot.xAxis (Axis.linearX (Just "acceleration") |> Axis.ticks 5 |> Axis.continuousDomain ( 0, 30 ) |> Axis.orientTop |> Axis.labelAngle angle)
        |> Plot.yAxis (Axis.linearY (Just "displacement") |> Axis.ticks 5 |> Axis.continuousDomain ( 0, 50 ) |> Axis.orientRight |> Axis.labelAngle angle)
        |> Plot.width 768
        |> Plot.height 600
        |> Plot.layer carPoint


scatterPlotFacetted : Plot Car Float Float Int String
scatterPlotFacetted =
    scatterPlot
        |> Plot.facetGrid
            { row = Field.scalar { name = Just "cylinders", extract = .cylinders }
            , column = Field.scalar { name = Just "origin", extract = .origin }
            }
        |> Plot.facetColumnFormat toShortCountry
        |> Plot.facetRowFormat (\n -> toString n ++ " cylinders")


carPoint : Encoding Car Float Float
carPoint =
    Encoding.point
        { x = Field.scalar { name = Just "acceleration", extract = .acceleration } |> Channel.positional
        , y = Field.maybeScalar { name = Just "miles per gallon", extract = .milesPerGallon } |> Channel.positional
        }
        |> Encoding.fill fillColorOrigin
        |> Encoding.fillOpacityConstant 0.5


fillColorOrigin : Channel.ColorChannel { a | origin : String }
fillColorOrigin =
    Channel.color
        { formatDomain = toShortCountry
        , scale = Scale.category10 [ "USA", "Japan", "Europe" ]
        , field = Field.scalar { name = Just "origin", extract = .origin }
        }


toShortCountry : String -> String
toShortCountry str =
    if str == "Europe" then
        "EUR"
    else if str == "Japan" then
        "JPN"
    else
        str


angle : number
angle =
    0


{-| Another scatter plot with size encoding
-}
scatterPlot2 : Plot Car Float Float facetRow facetColumn
scatterPlot2 =
    Plot.plot (Just "Horsepower vs. Miles per Gallon")
        |> Plot.xAxis (Axis.linearX (Just "horsepower") |> Axis.ticks 8 |> Axis.continuousDomain ( 0, 240 ) |> Axis.labelAngle angle)
        |> Plot.yAxis (Axis.linearY (Just "miles per gallon") |> Axis.ticks 5 |> Axis.continuousDomain ( 0, 50 ) |> Axis.labelAngle angle)
        |> Plot.width 768
        |> Plot.height 600
        |> Plot.layer carPoint2


carPoint2 : Encoding Car Float Float
carPoint2 =
    Encoding.point
        { x = Field.maybeScalar { name = (Just "horsepower"), extract = .horsepower >> Maybe.map toFloat } |> Channel.positional
        , y = Field.maybeScalar { name = (Just "miles per gallon"), extract = .milesPerGallon } |> Channel.positional
        }
        |> Encoding.fill fillColorOrigin
        |> Encoding.fillOpacityConstant 0.5
        |> Encoding.size sizeAcceleration


sizeAcceleration : Channel.FloatChannel { a | acceleration : Float }
sizeAcceleration =
    Channel.float
        { formatDomain = twoDecimalPlaces
        , scale = Scale.linear { domain = ( 10, 30 ), range = ( 10, 400 ) }
        , field = Field.scalar { name = Just "acceleration", extract = .acceleration }
        }


{-| Bar plot of average miles per gallon by origin
-}
barPlot : Plot Car String Float facetRow facetColumn
barPlot =
    Plot.plot (Just "Average MPG by origin")
        |> Plot.xAxis (Axis.bandX (Just "Origin") |> Axis.labelFormat toShortCountry)
        |> Plot.yAxis (Axis.linearY (Just "Average Miles Per Gallon") |> Axis.ticks 4)
        |> Plot.width 768
        |> Plot.height 600
        |> Plot.layer bar


bar : Encoding Car String Float
bar =
    Encoding.bar
        { x = Field.scalar { name = Just "Origin", extract = .origin } |> Channel.positional
        , width = 800
        , height = Field.maybeAggregate { name = Nothing, extract = averageMilesPerGallon } |> Channel.positional
        }
        |> Encoding.fill fillColorOrigin


barPlotRow : Plot Car String Float Int facetColumn
barPlotRow =
    barPlot
        |> Plot.facetRowWrap { row = Field.scalar { name = Nothing, extract = .cylinders }, maxPerRow = 5 }
        |> Plot.facetRowFormat (\n -> toString n ++ " cylinders")


barPlotColumn : Plot Car String Float facetRow Int
barPlotColumn =
    barPlot
        |> Plot.facetColumnWrap { column = Field.scalar { name = Nothing, extract = .cylinders }, maxPerColumn = 5 }
        |> Plot.facetColumnFormat (\n -> toString n ++ " cylinders")


averageMilesPerGallon : List Car -> Maybe Float
averageMilesPerGallon cars =
    case List.filterMap .milesPerGallon cars of
        [] ->
            Nothing

        ys ->
            let
                ( n, sum ) =
                    List.foldl (\mpg ( n, total ) -> ( n + 1, total + mpg )) ( 0, 0.0 ) ys
            in
                Just <| sum / toFloat n



-- Birdstrike plots ------------------------------------------------------------


birdBarPlot : Plot Birdstrike String Float facetRow facetColumn
birdBarPlot =
    Plot.plot (Just "Total cost by species")
        |> Plot.xAxis (Axis.customBandX (Just "Species") |> Axis.labelAngle (pi / 2) |> Axis.labelFormat (truncateString 8))
        |> Plot.yAxis (Axis.sqrtY (Just "Total Cost") |> Axis.continuousDomain ( 0, 2.0e6 ) |> Axis.labelFormat formatMoney)
        |> Plot.width 768
        |> Plot.height 600
        |> Plot.layer birdBar


birdBar : Encoding Birdstrike String Float
birdBar =
    Encoding.bar
        { x = Field.scalar { name = Just "Species", extract = .wildlifeSpecies } |> Channel.positional
        , width = 200
        , height = Field.maybeAggregate { name = Just "Total Cost", extract = sumBy .costTotal } |> Channel.positional
        }


sumBy : (a -> Float) -> List a -> Maybe Float
sumBy f xs =
    let
        ttl =
            List.foldl (\x total -> total + f x) 0.0 xs
    in
        if ttl == 0 then
            Nothing
        else
            Just ttl


truncateString : Int -> String -> String
truncateString n str =
    if String.length str <= n then
        str
    else
        String.slice 0 n str ++ ".."


formatMoney : Float -> String
formatMoney value =
    let
        pow =
            floor <| logBase 10 value
    in
        if pow < 3 then
            "$" ++ toString value
        else if pow < 6 then
            "$" ++ (twoDecimalPlaces (value / 1000)) ++ "k"
        else if pow < 9 then
            "$" ++ (twoDecimalPlaces (value / 1000000)) ++ "mn"
        else if pow < 12 then
            "$" ++ (twoDecimalPlaces (value / 10 ^ 9)) ++ "bn"
        else
            "$" ++ (twoDecimalPlaces (value / 10 ^ 12)) ++ "tn"


twoDecimalPlaces : Float -> String
twoDecimalPlaces number =
    floor (number * 100)
        |> toFloat
        |> \x ->
            toString (x / 100)



-- Other examples --------------------------------------------------------------


{-| Vector field example
-}
vectorFieldPlot : Plot { a | direction : Float, x : Float, y : Float, magnitude : Float } Float Float facetRow facetColumn
vectorFieldPlot =
    Plot.plot (Just "A portion of the vector field for F(-y,x)")
        |> Plot.xAxis (Axis.linearX (Just "x"))
        |> Plot.yAxis (Axis.linearY (Just "y"))
        |> Plot.width 768
        |> Plot.height 600
        |> Plot.layer vectorSymbol


vectorSymbol :
    Encoding.Encoding
        { a
            | direction : Float
            , x : comparable
            , y : comparable1
            , magnitude : Float
        }
        comparable
        comparable1
vectorSymbol =
    Encoding.arrow { x = vectorX, y = vectorY }
        |> Encoding.angle vectorDirection
        |> Encoding.size vectorMagnitude
        |> Encoding.fill vectorFill
        |> Encoding.stroke vectorFill


vectorX : Channel.PositionalChannel { a | x : comparable } comparable
vectorX =
    Channel.positional
        (Field.scalar { name = Just "x", extract = .x })


vectorY : Channel.PositionalChannel { a | y : comparable } comparable
vectorY =
    Channel.positional
        (Field.scalar { name = Just "y", extract = .y })


vectorMagnitude : Channel.FloatChannel { a | magnitude : Float }
vectorMagnitude =
    Channel.float
        { formatDomain = toString
        , scale = Scale.linear { domain = ( 0, 9 ), range = ( 1, 400.0 ) }
        , field = Field.scalar { name = Just "magnitude", extract = .magnitude }
        }


vectorMagnitude2 : Channel.FloatChannel { a | magnitude : Float }
vectorMagnitude2 =
    Channel.float
        { formatDomain = toString
        , scale = Scale.linear { domain = ( 0, 9 ), range = ( 1, 3 ) }
        , field = Field.scalar { name = Just "magnitude", extract = .magnitude }
        }


vectorFill : Channel.ColorChannel { a | magnitude : Float }
vectorFill =
    Channel.color
        { formatDomain = toString
        , scale = Scale.rgb { domain = ( 0, 9 ), range = ( Color.green, Color.red ) }
        , field = Field.scalar { name = Just "magnitude", extract = .magnitude }
        }


vectorDirection : Channel.FloatChannel { a | direction : Float }
vectorDirection =
    Channel.float
        { formatDomain = \radian -> toString (floor <| radianToDegree radian) ++ "°"
        , scale = Scale.linear { domain = ( 0, 2.0 * pi ), range = ( 0, 2.0 * pi ) }
        , field = Field.scalar { name = Just "direction", extract = .direction }
        }


radianToDegree : Float -> Float
radianToDegree x =
    x * 180.0 / Basics.pi


type alias VectorFieldData =
    { x : Float, y : Float, magnitude : Float, direction : Float }


{-| F(x,y) = (-y, x)
-}
vectorFromPos : Int -> Int -> VectorFieldData
vectorFromPos xIn yIn =
    let
        ( x, y ) =
            ( toFloat -yIn, toFloat xIn )

        magnitude =
            sqrt (x * x + y * y)

        {- vector direction in Radians -}
        direction =
            norm <| atan2 -y x
    in
        VectorFieldData x y magnitude direction


norm : Float -> Float
norm rads =
    rads - turns (toFloat (floor (rads / (2 * pi))))


vectorFieldData : List VectorFieldData
vectorFieldData =
    List.range -6 6
        |> List.concatMap
            (\x ->
                List.range -6 6
                    |> List.map (\y -> vectorFromPos x y)
            )



-- Stream graph ----------------------------------------------------------------


{-| TODO: implement streamgraph example
-}
streamData : List (List { high : Float, low : Float, ticker : String, x : number })
streamData =
    [ { x = 0, high = 177.38, low = 176.42, ticker = "AAPL" }
    , { x = 1, high = 176.19, low = 174.83, ticker = "AAPL" }
    , { x = 2, high = 175.84, low = 173.85, ticker = "AAPL" }
    , { x = 3, high = 175.0, low = 173.04, ticker = "AAPL" }
    , { x = 4, high = 173.92, low = 171.7, ticker = "AAPL" }
    , { x = 5, high = 174.0, low = 171.53, ticker = "AAPL" }
    , { x = 6, high = 173.09, low = 169.85, ticker = "AAPL" }
    , { x = 7, high = 172.48, low = 168.2, ticker = "AAPL" }
    , { x = 8, high = 174.23, low = 172.08, ticker = "AAPL" }
    , { x = 9, high = 172.01, low = 164.77, ticker = "AAPL" }
    , { x = 10, high = 168.75, low = 164.88, ticker = "AAPL" }
    , { x = 11, high = 168.94, low = 164.47, ticker = "AAPL" }
    , { x = 0, high = 167.36, low = 165.67, ticker = "FB" }
    , { x = 1, high = 165.78, low = 163.39, ticker = "FB" }
    , { x = 2, high = 165.7, low = 163.77, ticker = "FB" }
    , { x = 3, high = 167.45, low = 163.1, ticker = "FB" }
    , { x = 4, high = 168.65, low = 163.25, ticker = "FB" }
    , { x = 5, high = 165.98, low = 157.01, ticker = "FB" }
    , { x = 6, high = 160.53, low = 156.04, ticker = "FB" }
    , { x = 7, high = 161.42, low = 156.81, ticker = "FB" }
    , { x = 8, high = 161.57, low = 156.65, ticker = "FB" }
    , { x = 9, high = 155.56, low = 150.51, ticker = "FB" }
    , { x = 10, high = 157.39, low = 150.81, ticker = "FB" }
    , { x = 11, high = 159.2, low = 154.11, ticker = "FB" }
    , { x = 0, high = 288.47, low = 282.51, ticker = "TSLA" }
    , { x = 1, high = 299.66, low = 289.01, ticker = "TSLA" }
    , { x = 2, high = 303.95, low = 295.98, ticker = "TSLA" }
    , { x = 3, high = 303.95, low = 293.68, ticker = "TSLA" }
    , { x = 4, high = 308.98, low = 299.66, ticker = "TSLA" }
    , { x = 5, high = 307.1, low = 293.68, ticker = "TSLA" }
    , { x = 6, high = 309.5, low = 289.21, ticker = "TSLA" }
    , { x = 7, high = 309.28, low = 295.5, ticker = "TSLA" }
    , { x = 8, high = 306.26, low = 288.2, ticker = "TSLA" }
    , { x = 9, high = 288.37, low = 252.0, ticker = "TSLA" }
    , { x = 10, high = 273.35, low = 254.49, ticker = "TSLA" }
    , { x = 11, high = 260.33, low = 244.59, ticker = "TSLA" }
    ]
        |> groupBy .x


groupBy : (a -> comparable) -> List a -> List (List a)
groupBy project xs =
    xs
        |> List.sortBy project
        |> groupByHelper [] [] project Nothing


groupByHelper : List (List a) -> List a -> (a -> b) -> Maybe b -> List a -> List (List a)
groupByHelper accu currentAccu project maybeKey xs =
    case xs of
        [] ->
            List.reverse (currentAccu :: accu)

        next :: rest ->
            if Maybe.map ((==) (project next)) maybeKey |> Maybe.withDefault False then
                groupByHelper accu (next :: currentAccu) project maybeKey rest
            else
                let
                    newKey =
                        Just <| project next

                    newAccu =
                        (List.reverse currentAccu) :: accu
                in
                    groupByHelper newAccu [] project newKey rest



-- Trail -----------------------------------------------------------------------


trailPlot : Plot { a | w : Maybe Float, x : Float, y : Float } Float Float facetRow facetColumn
trailPlot =
    Plot.plot (Just "Trail mark example")
        |> Plot.xAxis (Axis.linearX (Just "x") |> Axis.continuousDomain ( 0, 12 ))
        |> Plot.yAxis (Axis.linearY (Just "y") |> Axis.continuousDomain ( 0, 8 ))
        |> Plot.width 768
        |> Plot.height 600
        |> Plot.layer trail


trailX : Channel.PositionalChannel { a | x : comparableDomain } comparableDomain
trailX =
    Field.vector { name = Nothing, extract = List.map .x } |> Channel.positional


trailY : Channel.PositionalChannel { a | y : comparableDomain } comparableDomain
trailY =
    Field.vector { name = Nothing, extract = List.map .y } |> Channel.positional


trailWidth : Channel.FloatChannel { a | w : Maybe Float }
trailWidth =
    Channel.float
        { formatDomain = toString
        , scale = Scale.linear { domain = ( 0, 10 ), range = ( 10, 40 ) }
        , field = Field.maybeVector { name = Nothing, extract = List.map .w }
        }


trail : Encoding.Encoding { a | w : Maybe Float, x : comparableDomain, y : comparableDomain1 } comparableDomain comparableDomain1
trail =
    Encoding.trail
        { x = trailX
        , y = trailY
        , width = trailWidth
        , onMissing = SkipMissing
        }


trailData : List { w : Maybe Float, x : Float, y : Float }
trailData =
    [ { x = 2, y = 2, w = Just 4 }
    , { x = 4, y = 2, w = Just 6 }
    , { x = 7.4, y = 4.5, w = Just 8 }
    , { x = 8.6, y = 4.0, w = Just 11.5 }
    , { x = 10, y = 6.0, w = Just 3 }
    ]



-- Line -----------------------------------------------------------------


linePlot : Plot ( Float, Float ) Float Float facetRow facetColumn
linePlot =
    Plot.plot (Just "Interpolation methods")
        |> Plot.xAxis (Axis.linearX (Just "x") |> Axis.continuousDomain ( 0.0, 2.0 * pi ))
        |> Plot.yAxis (Axis.linearY (Just "y") |> Axis.continuousDomain ( -1.0, 1.0 ))
        |> Plot.width 768
        |> Plot.height 600
        |> Plot.layer point
        |> \plot -> List.foldl (\layer plot -> Plot.layer layer plot) plot lines


methods : List ( Interpolate, Color )
methods =
    [ ( Linear, Color.rgb 31 119 180 )
    , ( Basis, Color.rgb 255 127 14 )
    , ( Bundle 0.5, Color.rgb 255 127 14 )
    , ( Cardinal 0.5, Color.rgb 44 160 44 )
    , ( CatmullRom 0.5, Color.rgb 214 39 40 )
    , ( Monotone, Color.rgb 148 103 189 )
    , ( Natural, Color.rgb 140 86 75 )
    , ( Step, Color.rgb 227 119 194 )
    , ( StepAfter, Color.rgb 17 127 127 )
    , ( StepBefore, Color.rgb 188 189 34 )
    ]


lines : List (Encoding ( comparable, comparableDomain ) comparable comparableDomain)
lines =
    methods
        |> List.map
            (\( method, color ) ->
                Encoding.line { x = lineX, y = lineY, interpolate = method, onMissing = SkipMissing }
                    |> Encoding.strokeConstant color
                    |> Encoding.strokeOpacityConstant 0.5
            )


lineX : Channel.PositionalChannel ( comparable, a2 ) comparable
lineX =
    Field.vector { name = Just "x", extract = List.sortBy Tuple.first >> List.map Tuple.first }
        |> Channel.positional


lineY : Channel.PositionalChannel ( comparable, comparableDomain ) comparableDomain
lineY =
    Field.vector { name = Just "sin x", extract = List.sortBy Tuple.first >> List.map Tuple.second }
        |> Channel.positional


pointX : Channel.PositionalChannel ( comparableDomain, a2 ) comparableDomain
pointX =
    Field.scalar { name = Just "x", extract = Tuple.first } |> Channel.positional


pointY : Channel.PositionalChannel ( a1, comparableDomain ) comparableDomain
pointY =
    Field.scalar { name = Just "sin x", extract = Tuple.second } |> Channel.positional


point : Encoding ( comparable, comparableDomain ) comparable comparableDomain
point =
    Encoding.point { x = pointX, y = pointY }
        |> Encoding.sizeConstant 25
        |> Encoding.fillConstant Color.black


lineData : List ( Float, Float )
lineData =
    List.range 0 marks
        |> List.map
            (\i ->
                let
                    x =
                        step * toFloat i
                in
                    ( x, sin x )
            )


marks : number
marks =
    51


step : Float
step =
    (2 * pi) / 50.0



-- Aggregate by position -------------------------------------------------------


discreteData : List { n : Int, a : String, v : Float }
discreteData =
    [ 0, 1, 2, 3, 4 ]
        |> List.concatMap
            (\n ->
                [ "a", "b", "c", "d" ]
                    |> List.concatMap
                        (\a ->
                            let
                                vs =
                                    if a == "c" || n % 2 == 0 then
                                        [ 0.1, 0.2, 0.3 ]
                                    else
                                        [ 0.3, 0.8, 1.2 ]
                            in
                                vs
                                    |> List.map (\v -> { n = n, a = a, v = v })
                        )
            )


totalValue : Field.Field { a | v : number } number
totalValue =
    Field.aggregate { name = Just "total value", extract = (List.map .v >> List.sum) }


boxFill : Channel.ColorChannel { a | v : Float }
boxFill =
    Channel.color
        { formatDomain = toString
        , scale = Scale.rgb { domain = ( 0.1, 4 ), range = ( Color.green, Color.red ) }
        , field = totalValue
        }


boxSize : Channel.FloatChannel { a | v : Float }
boxSize =
    Channel.float
        { formatDomain = toString
        , scale = Scale.linear { domain = ( 0.1, 4 ), range = ( 0, 4000 ) }
        , field = totalValue
        }


boxEncoding : Encoding { r | a : comparable, n : comparable1, v : Float } comparable1 comparable
boxEncoding =
    Encoding.square
        { x = Channel.positional <| Field.scalar { name = Nothing, extract = .n }
        , y = Channel.positional <| Field.scalar { name = Nothing, extract = .a }
        }
        -- |> Encoding.fillConstant Color.green
        |>
            Encoding.fill boxFill
        -- |> Encoding.stroke boxFill
        |>
            Encoding.size boxSize


boxPlot : Plot { r | a : String, n : comparable, v : Float } comparable String facetRow facetColumn
boxPlot =
    Plot.plot Nothing
        |> Plot.xAxis (Axis.bandX Nothing)
        |> Plot.yAxis (Axis.bandY Nothing |> Axis.labelFormat identity)
        |> Plot.width 768
        |> Plot.height 600
        |> Plot.layer boxEncoding



-- Aggragte on non-comparable field --------------------------------------------


boolData : List ( Bool, Float )
boolData =
    [ ( True, 0.2 ), ( False, 0.25 ), ( False, 0.25 ), ( True, 0.3 ) ]


compareBool : Bool -> Bool -> Order
compareBool a b =
    if a == b then
        EQ
    else if a then
        GT
    else
        LT


boolEncoding : Encoding ( Bool, Float ) Bool Float
boolEncoding =
    Encoding.bar
        { x = Channel.positionalCompareWith { compareWith = compareBool, field = Field.scalar { name = Just "outcome", extract = Tuple.first } }
        , width = 400
        , height = Channel.positional <| Field.aggregate { name = Just "probability mass", extract = List.map Tuple.second >> List.sum }
        }


boolPlot : Plot ( Bool, Float ) Bool Float facetRow facetColumn
boolPlot =
    Plot.plot (Just "A bar chart aggregating over non-comparable fields")
        |> Plot.xAxis (Axis.customBandX (Just "outcome"))
        |> Plot.yAxis (Axis.linearY (Just "probability mass") |> Axis.continuousDomain ( 0, 1 ))
        |> Plot.width 768
        |> Plot.height 600
        |> Plot.layer boolEncoding



-- Area ------------------------------------------------------------------------


areaBeginNew : Plot { a | y : Maybe Float, x : Float } Float Float facetRow facetColumn
areaBeginNew =
    Plot.plot (Just "An area chart with `BeginNew` behaviour")
        |> Plot.xAxis (Axis.linearX (Just "x") |> Axis.continuousDomain ( 0, 11 ))
        |> Plot.yAxis (Axis.linearY (Just "y") |> Axis.continuousDomain ( 0, 8 ))
        |> Plot.width 768
        |> Plot.height 600
        |> Plot.layer (areaEncoding BeginNew)
        |> Plot.layer (areaLine BeginNew)
        |> Plot.layer areaPoint


areaSkipMissing : Plot { a | y : Maybe Float, x : Float } Float Float facetRow facetColumn
areaSkipMissing =
    Plot.plot (Just "An area chart with `SkipMissing` behaviour")
        |> Plot.xAxis (Axis.linearX (Just "x") |> Axis.continuousDomain ( 0, 11 ))
        |> Plot.yAxis (Axis.linearY (Just "y") |> Axis.continuousDomain ( 0, 8 ))
        |> Plot.width 768
        |> Plot.height 600
        |> Plot.layer (areaEncoding SkipMissing)
        |> Plot.layer (areaLine SkipMissing)
        |> Plot.layer areaPoint


areaPoint : Encoding { a | x : comparable, y : Maybe comparable1 } comparable comparable1
areaPoint =
    Encoding.point
        { x = Field.scalar { name = Just "x", extract = .x } |> Channel.positional
        , y = Field.maybeScalar { name = (Just "y"), extract = .y } |> Channel.positional
        }
        |> Encoding.sizeConstant 25
        |> Encoding.fillConstant Color.black


areaLine : Behaviour -> Encoding { a | y : Maybe comparable1, x : comparable } comparable comparable1
areaLine behaviour =
    Encoding.line { x = areaX, y = areaY, interpolate = Monotone, onMissing = behaviour }
        |> Encoding.strokeConstant (Color.rgb 255 127 14)
        |> Encoding.strokeWidthConstant 2


areaEncoding : Behaviour -> Encoding { a | y : Maybe number, x : comparable } comparable number
areaEncoding behaviour =
    Encoding.hArea { x = areaX, y = areaY, interpolate = Monotone, onMissing = behaviour }
        |> Encoding.fillConstant (Color.rgb 255 127 14)
        |> Encoding.fillOpacityConstant 0.7


areaX : Channel.PositionalChannel { a | x : comparable } comparable
areaX =
    Field.vector { name = Just "x", extract = List.sortBy .x >> List.map .x }
        |> Channel.positional


areaY : Channel.PositionalChannel { a | x : comparable, y : Maybe comparableDomain } comparableDomain
areaY =
    Field.maybeVector { name = Just "y", extract = (List.sortBy .x >> List.map .y) }
        |> Channel.positional


areaData : List { x : Float, y : Maybe Float }
areaData =
    [ { x = 1, y = Just 2 }
    , { x = 3, y = Just 5 }
    , { x = 4.5, y = Just 4.8 }
    , { x = 5.5, y = Nothing }
    , { x = 7.2, y = Just 6.4 }
    , { x = 8, y = Just 7.4 }
    , { x = 10, y = Just 5.3 }
    ]
