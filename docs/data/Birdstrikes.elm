module Data.Birdstrikes exposing (Birdstrike, Damage, compareDamage, PhaseOfFlight, comparePhaseOfFlight, Size, compareSize, TimeOfDay, compareTimeOfDay, birdstrikes)

import Date exposing (Date)
import Date exposing (Date)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Task exposing (Task)


type alias Birdstrike =
    { airportName : String
    , aircraftMakeModel : String
    , effectAmountOfDamage : Damage
    , flightDate : Date
    , aircraftAirlineOperator : String
    , originState : String
    , whenPhaseOfFlight : PhaseOfFlight
    , wildlifeSize : Size
    , wildlifeSpecies : String
    , whenTimeOfDay : TimeOfDay
    , costOther : Float
    , costRepair : Float
    , costTotal : Float
    , speedIASinKnots : Maybe Float
    }


birdstrikes : () -> Task Http.Error (List Birdstrike)
birdstrikes () =
    Http.get
        "https://enetsee.github.io/facet-plot-alpha/data/birdstrikes.json"
        (Decode.list decode)
        |> Http.toTask


decode : Decoder Birdstrike
decode =
    Decode.decode Birdstrike
        |> Decode.required "Airport__Name" Decode.string
        |> Decode.required "Aircraft__Make_Model" Decode.string
        |> Decode.required "Effect__Amount_of_damage" decodeDamage
        |> Decode.required "Flight_Date" decodeDate
        |> Decode.required "Aircraft__Airline_Operator" Decode.string
        |> Decode.required "Origin_State" Decode.string
        |> Decode.required "When__Phase_of_flight" decodePhaseOfFlight
        |> Decode.required "Wildlife__Size" decodeSize
        |> Decode.required "Wildlife__Species" Decode.string
        |> Decode.required "When__Time_of_day" decodeTimeOfDay
        |> Decode.required "Cost__Other" Decode.float
        |> Decode.required "Cost__Repair" Decode.float
        |> Decode.required "Cost__Total_$" Decode.float
        |> Decode.required "Speed_IAS_in_knots" (Decode.nullable Decode.float)


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


decodeResult : Result String a -> Decoder a
decodeResult result =
    case result of
        Ok x ->
            Decode.succeed x

        Err x ->
            Decode.fail x



-- Time of Day --


type TimeOfDay
    = Dawn
    | Day
    | Dusk
    | Night


decodeTimeOfDay : Decoder TimeOfDay
decodeTimeOfDay =
    Decode.string
        |> Decode.andThen (timeOfDayFromString >> decodeResult)


timeOfDayFromString : String -> Result String TimeOfDay
timeOfDayFromString str =
    case str of
        "Dawn" ->
            Ok Dawn

        "Day" ->
            Ok Day

        "Dusk" ->
            Ok Dusk

        "Night" ->
            Ok Night

        _ ->
            Err str


timeOfDayOrd : TimeOfDay -> number
timeOfDayOrd timeOfDay =
    case timeOfDay of
        Dawn ->
            0

        Day ->
            1

        Dusk ->
            2

        Night ->
            3


compareTimeOfDay : TimeOfDay -> TimeOfDay -> Order
compareTimeOfDay d1 d2 =
    compare (timeOfDayOrd d1) (timeOfDayOrd d2)



-- Damage --


type Damage
    = None
    | B
    | C
    | Minor
    | DamageMedium
    | Substantial


decodeDamage : Decoder Damage
decodeDamage =
    Decode.string
        |> Decode.andThen (damageFromString >> decodeResult)


damageFromString : String -> Result String Damage
damageFromString str =
    case str of
        "None" ->
            Ok None

        "B" ->
            Ok B

        "C" ->
            Ok C

        "Minor" ->
            Ok Minor

        "Medium" ->
            Ok DamageMedium

        "Substantial" ->
            Ok Substantial

        _ ->
            Err str


compareDamage : Damage -> Damage -> Order
compareDamage d1 d2 =
    compare (damageOrd d1) (damageOrd d2)


damageOrd : Damage -> number
damageOrd damage =
    case damage of
        None ->
            0

        B ->
            1

        C ->
            2

        Minor ->
            3

        DamageMedium ->
            4

        Substantial ->
            5



-- Phase of fligh --


type PhaseOfFlight
    = Parked
    | Taxi
    | TakeoffRun
    | Climb
    | Approach
    | Descent
    | LandingRoll


decodePhaseOfFlight : Decoder PhaseOfFlight
decodePhaseOfFlight =
    Decode.string
        |> Decode.andThen (phaseOfFlightFromString >> decodeResult)


phaseOfFlightFromString : String -> Result String PhaseOfFlight
phaseOfFlightFromString str =
    case str of
        "Parked" ->
            Ok Parked

        "Taxi" ->
            Ok Taxi

        "Take-off run" ->
            Ok TakeoffRun

        "Climb" ->
            Ok Climb

        "Approach" ->
            Ok Approach

        "Descent" ->
            Ok Descent

        "Landing Roll" ->
            Ok LandingRoll

        _ ->
            Err str


comparePhaseOfFlight : PhaseOfFlight -> PhaseOfFlight -> Order
comparePhaseOfFlight d1 d2 =
    compare (phaseOfFlightOrd d1) (phaseOfFlightOrd d2)


phaseOfFlightOrd : PhaseOfFlight -> number
phaseOfFlightOrd phaseOfFlight =
    case phaseOfFlight of
        Parked ->
            0

        Taxi ->
            1

        TakeoffRun ->
            2

        Climb ->
            3

        Approach ->
            4

        Descent ->
            5

        LandingRoll ->
            6



-- Size --


type Size
    = Small
    | Medium
    | Large


decodeSize : Decoder Size
decodeSize =
    Decode.string
        |> Decode.andThen (sizeFromStr >> decodeResult)


sizeFromStr : String -> Result String Size
sizeFromStr str =
    case str of
        "Small" ->
            Ok Small

        "Medium" ->
            Ok Medium

        "Large" ->
            Ok Large

        _ ->
            Err str


compareSize : Size -> Size -> Order
compareSize d1 d2 =
    compare (sizeOrd d1) (sizeOrd d2)


sizeOrd : Size -> number
sizeOrd size =
    case size of
        Small ->
            0

        Medium ->
            1

        Large ->
            2
