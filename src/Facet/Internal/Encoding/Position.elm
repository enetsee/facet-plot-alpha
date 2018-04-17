module Facet.Internal.Encoding.Position
    exposing
        ( Position(..)
        , extract
        , extractVector
        , summarize
        , containsAggregate
        , containsVector
        , compareAt
        , equalAt
        , isConstant
        )

import Facet.Internal.Channel as Channel exposing (PositionalChannel, FloatChannel)
import Facet.Internal.Field as Field
import Facet.Scale as Scale exposing (Scale)
import Facet.Scenegraph.Position as SG
import Facet.List.Extra as List


{-| TODO: this needs overhauling when the corresponding changes are made in `Scenegraph`
-}
type Position data domain
    = PrimarySecondary (PositionalChannel data domain) (PositionalChannel data domain)
    | PrimaryExtent (PositionalChannel data domain) (FloatChannel data)
    | SecondaryExtent (PositionalChannel data domain) (FloatChannel data)
    | CenterExtent (PositionalChannel data domain) (FloatChannel data)


isConstant : Position data domain -> Bool
isConstant position =
    case position of
        PrimarySecondary x x2 ->
            Field.isConstant x.field && Field.isConstant x2.field

        PrimaryExtent x _ ->
            Field.isConstant x.field

        SecondaryExtent x _ ->
            Field.isConstant x.field

        CenterExtent x _ ->
            Field.isConstant x.field


compareAt : Position data domain -> data -> data -> Order
compareAt position d1 d2 =
    case position of
        PrimarySecondary position1 position2 ->
            case Channel.compareAt position1 d1 d2 of
                EQ ->
                    Channel.compareAt position2 d1 d2

                otherwise ->
                    otherwise

        PrimaryExtent position extent ->
            case Channel.compareAt position d1 d2 of
                EQ ->
                    Channel.compareAt extent d1 d2

                otherwise ->
                    otherwise

        SecondaryExtent position extent ->
            case Channel.compareAt position d1 d2 of
                EQ ->
                    Channel.compareAt extent d1 d2

                otherwise ->
                    otherwise

        CenterExtent position extent ->
            case Channel.compareAt position d1 d2 of
                EQ ->
                    Channel.compareAt extent d1 d2

                otherwise ->
                    otherwise


equalAt : Position data domain -> data -> data -> Bool
equalAt position d1 d2 =
    case position of
        PrimarySecondary position1 position2 ->
            Channel.equalAt position1 d1 d2
                && Channel.equalAt position2 d1 d2

        PrimaryExtent position extent ->
            Channel.equalAt position d1 d2
                && Channel.equalAt extent d1 d2

        SecondaryExtent position extent ->
            Channel.equalAt position d1 d2
                && Channel.equalAt extent d1 d2

        CenterExtent position extent ->
            Channel.equalAt position d1 d2
                && Channel.equalAt extent d1 d2


extract : Scale domain Float -> Position data domain -> data -> Maybe SG.Position
extract scale position datum =
    case position of
        PrimarySecondary primary secondary ->
            Maybe.map2
                SG.PrimarySecondary
                (Field.extract primary.field datum |> Maybe.andThen (Scale.scale scale))
                (Field.extract secondary.field datum |> Maybe.andThen (Scale.scale scale))

        PrimaryExtent { field } extent ->
            Maybe.map2
                SG.PrimaryExtent
                (Field.extract field datum |> Maybe.andThen (Scale.scale scale))
                (Channel.extract extent datum)

        SecondaryExtent { field } extent ->
            Maybe.map2
                SG.SecondaryExtent
                (Field.extract field datum |> Maybe.andThen (Scale.scale scale))
                (Channel.extract extent datum)

        CenterExtent { field } extent ->
            Maybe.map2
                SG.CenterExtent
                (Field.extract field datum |> Maybe.andThen (Scale.scale scale))
                (Channel.extract extent datum)


extractVector : Scale domain Float -> Position data domain -> List data -> List (Maybe SG.Position)
extractVector scale position data =
    case position of
        PrimarySecondary primary secondary ->
            let
                xs =
                    data
                        |> Field.extractVector primary.field
                        |> List.map (Maybe.andThen (Scale.scale scale))

                ys =
                    data
                        |> Field.extractVector secondary.field
                        |> List.map (Maybe.andThen (Scale.scale scale))
            in
                List.zipCycle xs ys
                    |> List.map (\( x, y ) -> Maybe.map2 SG.PrimarySecondary x y)

        PrimaryExtent { field } extent ->
            let
                xs =
                    data
                        |> Field.extractVector field
                        |> List.map (Maybe.andThen (Scale.scale scale))

                ys =
                    data |> Channel.extractVector extent
            in
                List.zipCycle xs ys
                    |> List.map (\( x, y ) -> Maybe.map2 SG.PrimaryExtent x y)

        SecondaryExtent { field } extent ->
            let
                xs =
                    data
                        |> Field.extractVector field
                        |> List.map (Maybe.andThen (Scale.scale scale))

                ys =
                    data |> Channel.extractVector extent
            in
                List.zipCycle xs ys
                    |> List.map (\( x, y ) -> Maybe.map2 SG.SecondaryExtent x y)

        CenterExtent { field } extent ->
            let
                xs =
                    data
                        |> Field.extractVector field
                        |> List.map (Maybe.andThen (Scale.scale scale))

                ys =
                    data |> Channel.extractVector extent
            in
                List.zipCycle xs ys
                    |> List.map (\( x, y ) -> Maybe.map2 SG.CenterExtent x y)


summarize : Scale domain Float -> Position data domain -> List data -> Maybe SG.Position
summarize scale position data =
    case position of
        PrimarySecondary primary secondary ->
            Maybe.map2
                SG.PrimarySecondary
                (Field.summarize primary.field data |> Maybe.andThen (Scale.scale scale))
                (Field.summarize secondary.field data |> Maybe.andThen (Scale.scale scale))

        PrimaryExtent { field } extent ->
            Maybe.map2
                SG.PrimaryExtent
                (Field.summarize field data |> Maybe.andThen (Scale.scale scale))
                (Channel.summarize extent data)

        SecondaryExtent { field } extent ->
            Maybe.map2
                SG.SecondaryExtent
                (Field.summarize field data |> Maybe.andThen (Scale.scale scale))
                (Channel.summarize extent data)

        CenterExtent { field } extent ->
            Maybe.map2
                SG.CenterExtent
                (Field.summarize field data |> Maybe.andThen (Scale.scale scale))
                (Channel.summarize extent data)


containsAggregate : Position data domain -> Bool
containsAggregate position =
    case position of
        PrimarySecondary primary secondary ->
            primary.isAggregate || secondary.isAggregate

        PrimaryExtent primary extent ->
            primary.isAggregate || extent.isAggregate

        SecondaryExtent secondary extent ->
            secondary.isAggregate || extent.isAggregate

        CenterExtent center extent ->
            center.isAggregate || extent.isAggregate


containsVector : Position data domain -> Bool
containsVector position =
    case position of
        PrimarySecondary primary secondary ->
            primary.isVector || secondary.isVector

        PrimaryExtent primary extent ->
            primary.isVector || extent.isVector

        SecondaryExtent secondary extent ->
            secondary.isVector || extent.isVector

        CenterExtent center extent ->
            center.isVector || extent.isVector
