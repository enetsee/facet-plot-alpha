module Data.Cars exposing (Car, cars)

import Date exposing (Date)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Task exposing (Task)


cars : () -> Task Http.Error (List Car)
cars () =
    Http.get
        "http://localhost:8000/data/cars.json"
        (Decode.list decode)
        |> Http.toTask


{-| Example data from https://vega.github.io/vega-editor/app/data/cars.json
-}
type alias Car =
    { name : String
    , milesPerGallon : Maybe Float
    , cylinders : Int
    , displacement : Float
    , horsepower : Maybe Int
    , weightInLbs : Int
    , acceleration : Float
    , year : Maybe Date
    , origin : String
    }


decode : Decoder Car
decode =
    Decode.decode Car
        |> Decode.required "Name" Decode.string
        |> Decode.required "Miles_per_Gallon" (Decode.nullable Decode.float)
        |> Decode.required "Cylinders" Decode.int
        |> Decode.required "Displacement" Decode.float
        |> Decode.required "Horsepower" (Decode.nullable Decode.int)
        |> Decode.required "Weight_in_lbs" Decode.int
        |> Decode.required "Acceleration" Decode.float
        |> Decode.required "Year" (Decode.nullable decodeDate)
        |> Decode.required "Origin" Decode.string


decodeDate : Decoder Date
decodeDate =
    Decode.andThen
        (\dateStr ->
            case Date.fromString dateStr of
                Ok d ->
                    Decode.succeed d

                Err err ->
                    Decode.fail err
        )
        Decode.string
