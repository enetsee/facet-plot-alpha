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
                    , ( compileWith cars scatterPlotReversed, "The same plot with the axes oriented on the top and left." )
                    , ( compileWith cars scatterPlotFacetted, "The same plot faceted by origin and number of cylenders." )
                    , ( compileWith cars barPlot, "A bar plot using an `AggregateField` to find the average miles per gallon." )
                    , ( compileWith cars barPlotRow, "The same bar plot faceted as a row." )
                    , ( compileWith cars barPlotColumn, "The same bar plot again now faceted as a column." )
                    ]

                birdstrikesScenegraphs =
                    [ ( compileWith birdstrikes birdBarPlot, "A bar plot with a square root scale to show skewed data." ) ]

                otherScenegraphs =
                    [ ( compileWith vectorFieldData vectorFieldPlot, "A symbol plot using the built-in arrow shape to represent a vector field. Note that all legends are generated from the plot declaration automatically." )
                    , ( compileWith trailData trailPlot, "A trail plot doing nothing particularly interesting." )
                    , ( compileWith lineData linePlot, "A line plot showing the different interpolation methods applied to the sin function." )
                    , ( compileWith areaData areaBeginNew, "An area plot showing the `BeginNew` behaviour when missing data (encoded as `Maybe`) is encountered ." )
                    , ( compileWith areaData areaSkipMissing, "The same area plot showing the `SkipMissing` behaviour when missing data (encoded as `Maybe`) is encountered ." )
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
        |> Plot.facetGrid (Field.scalar (Just "cylinders") .cylinders) (Field.scalar (Just "origin") .origin)
        |> Plot.facetColumnFormat toShortCountry
        |> Plot.facetRowFormat (\n -> toString n ++ " cylinders")


carPoint : Encoding Car Float Float
carPoint =
    Encoding.point
        (Field.scalar (Just "acceleration") .acceleration |> Channel.positional)
        (Field.maybeScalar (Just "miles per gallon") .milesPerGallon |> Channel.positional)
        |> Encoding.fill fillColorOrigin
        |> Encoding.fillOpacityConstant 0.5


fillColorOrigin : Channel.ColorChannel { a | origin : String }
fillColorOrigin =
    Field.scalar (Just "origin") .origin
        |> Channel.color toShortCountry (Scale.category10 [ "USA", "Japan", "Europe" ])


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
        (Field.scalar (Just "Origin") .origin |> Channel.positional)
        800
        (Field.maybeAggregate Nothing averageMilesPerGallon |> Channel.positional)
        |> Encoding.fill fillColorOrigin


barPlotRow : Plot Car String Float Int facetColumn
barPlotRow =
    barPlot
        |> Plot.facetRowWrap (Field.scalar Nothing .cylinders) 5
        |> Plot.facetRowFormat (\n -> toString n ++ " cylinders")


barPlotColumn : Plot Car String Float facetRow Int
barPlotColumn =
    barPlot
        |> Plot.facetColumnWrap (Field.scalar Nothing .cylinders) 5
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
        (Field.scalar (Just "Species") .wildlifeSpecies |> Channel.positional)
        200
        (Field.maybeAggregate (Just "Total Cost") (sumBy .costTotal) |> Channel.positional)


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
            "$" ++ toString (round (value / 1000)) ++ "k"
        else if pow < 9 then
            "$" ++ toString (round (value / 1000000)) ++ "mn"
        else if pow < 12 then
            "$" ++ toString (round (value / 10 ^ 9)) ++ "bn"
        else
            "$" ++ toString (round (value / 10 ^ 12)) ++ "tn"



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
    Encoding.arrow vectorX vectorY
        |> Encoding.angle vectorDirection
        |> Encoding.size vectorMagnitude
        |> Encoding.fill vectorFill
        |> Encoding.stroke vectorFill


vectorX : Channel.PositionalChannel { a | x : comparable } comparable
vectorX =
    Channel.positional
        (Field.scalar (Just "x") .x)


vectorY : Channel.PositionalChannel { a | y : comparable } comparable
vectorY =
    Channel.positional
        (Field.scalar (Just "y") .y)


vectorMagnitude : Channel.FloatChannel { a | magnitude : Float }
vectorMagnitude =
    Channel.float
        toString
        (Scale.linear ( 0, 9 ) ( 1, 400.0 ))
        (Field.scalar (Just "magnitude") .magnitude)


vectorMagnitude2 : Channel.FloatChannel { a | magnitude : Float }
vectorMagnitude2 =
    Channel.float
        toString
        (Scale.linear ( 0, 9 ) ( 1, 3 ))
        (Field.scalar (Just "magnitude") .magnitude)


vectorFill : Channel.ColorChannel { a | magnitude : Float }
vectorFill =
    Channel.color
        toString
        (Scale.rgb ( 0, 9 ) ( Color.green, Color.red ))
        (Field.scalar (Just "magnitude") .magnitude)


vectorDirection : Channel.FloatChannel { a | direction : Float }
vectorDirection =
    Channel.float
        (\radian -> toString (floor <| radianToDegree radian) ++ "°")
        (Scale.linear ( 0, 2.0 * pi ) ( 0, 2.0 * pi ))
        (Field.scalar (Just "direction") (.direction))


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
    Field.vector Nothing (List.map .x) |> Channel.positional


trailY : Channel.PositionalChannel { a | y : comparableDomain } comparableDomain
trailY =
    Field.vector Nothing (List.map .y) |> Channel.positional


trailWidth : Channel.FloatChannel { a | w : Maybe Float }
trailWidth =
    Field.maybeVector Nothing (List.map .w) |> Channel.float toString (Scale.linear ( 0, 10 ) ( 10, 40 ))


trail : Encoding.Encoding { a | w : Maybe Float, x : comparableDomain, y : comparableDomain1 } comparableDomain comparableDomain1
trail =
    Encoding.trail
        trailX
        trailY
        trailWidth
        SkipMissing


trailData : List { w : Maybe Float, x : Float, y : Float }
trailData =
    [ { x = 2, y = 2, w = Just 4 }
    , { x = 4, y = 2, w = Just 6 }
    , { x = 7.4, y = 4.5, w = Just 8 }
    , { x = 8.6, y = 4.0, w = Just 6.5 }
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
        |> List.map (\( method, color ) -> Encoding.line lineX lineY method SkipMissing |> Encoding.strokeConstant color |> Encoding.strokeOpacityConstant 0.5)


lineX : Channel.PositionalChannel ( comparable, a2 ) comparable
lineX =
    Field.vector (Just "x") (List.sortBy Tuple.first >> List.map Tuple.first)
        |> Channel.positional


lineY : Channel.PositionalChannel ( comparable, comparableDomain ) comparableDomain
lineY =
    Field.vector (Just "sin x") (List.sortBy Tuple.first >> List.map Tuple.second)
        |> Channel.positional


pointX : Channel.PositionalChannel ( comparableDomain, a2 ) comparableDomain
pointX =
    Field.scalar (Just "x") Tuple.first |> Channel.positional


pointY : Channel.PositionalChannel ( a1, comparableDomain ) comparableDomain
pointY =
    Field.scalar (Just "sin x") Tuple.second |> Channel.positional


point : Encoding ( comparable, comparableDomain ) comparable comparableDomain
point =
    Encoding.point pointX pointY
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



-- Area ------------------------------------------------------------------------


areaBase : Plot data Float Float facetRow facetColumn
areaBase =
    Plot.plot (Just "An area chart with `BeginNew` behaviour")
        |> Plot.xAxis (Axis.linearX (Just "x") |> Axis.continuousDomain ( 0, 11 ))
        |> Plot.yAxis (Axis.linearY (Just "y") |> Axis.continuousDomain ( 0, 8 ))
        |> Plot.width 768
        |> Plot.height 600


areaBeginNew : Plot { a | y : Maybe Float, x : Float } Float Float facetRow facetColumn
areaBeginNew =
    areaBase
        |> Plot.layer (areaEncoding BeginNew)


areaSkipMissing : Plot { a | y : Maybe Float, x : Float } Float Float facetRow facetColumn
areaSkipMissing =
    areaBase
        |> Plot.layer (areaEncoding SkipMissing)


areaEncoding : Behaviour -> Encoding.Encoding { a | y : Maybe number, x : comparable } comparable number
areaEncoding behaviour =
    Encoding.hArea areaX areaY Monotone behaviour
        |> Encoding.fillConstant (Color.rgb 255 127 14)
        |> Encoding.fillOpacityConstant 0.7


areaX : Channel.PositionalChannel { a | x : comparable } comparable
areaX =
    Field.vector (Just "x") (List.sortBy .x >> List.map .x)
        |> Channel.positional


areaY : Channel.PositionalChannel { a | x : comparable, y : Maybe comparableDomain } comparableDomain
areaY =
    Field.maybeVector (Just "y") (List.sortBy .x >> List.map .y)
        |> Channel.positional


areaData : List { x : Float, y : Maybe Float }
areaData =
    [ { x = 1, y = Just 2 }
    , { x = 3, y = Just 5 }
    , { x = 4.5, y = Just 4.8 }
    , { x = 5.5, y = Nothing }
    , { x = 8, y = Just 7.4 }
    , { x = 10, y = Just 5.3 }
    ]
