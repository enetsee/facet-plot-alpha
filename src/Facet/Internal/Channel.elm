module Facet.Internal.Channel
    exposing
        ( PositionalChannel
        , ChannelWithLegend
        , AngleChannel
        , ColorChannel
        , FloatChannel
        , IntChannel
        , ShapeChannel
        , TextChannel
        , StrokeDashChannel
        , legend
        , extract
        , extractVector
        , summarize
        , isAggregate
        , isVector
        , compareAt
        , compareMaybeAt
        , equalAt
        , equalMaybeAt
        , positionalCompareWith
        , positional
        , channelCompareWith
        , channel
        , angleCompareWith
        , angle
        , colorCompareWith
        , color
        , floatCompareWith
        , float
        , intCompareWith
        , int
        , shapeCompareWith
        , shape
        , textCompareWith
        , text
        , strokeDashCompareWith
        , strokeDash
        )

import Color exposing (Color)
import Facet.Internal.Field as Field exposing (Field)
import Facet.Internal.Legend as Legend exposing (Legend)
import Facet.Maybe.Extra as Maybe
import Facet.Scale as Scale exposing (Scale)
import Facet.Scenegraph.Shape exposing (Shape)
import Facet.Scenegraph.Stroke exposing (StrokeDash)


{-|
   A channel to encode data as position
-}
type alias PositionalChannel data domain =
    { title : Maybe String
    , isAggregate : Bool
    , isVector : Bool
    , fieldEqual : data -> data -> Bool
    , fieldCompare : data -> data -> Order
    , field : Field data domain
    , displayAxis : Bool
    , compareDomain : domain -> domain -> Order
    , limits : Maybe ( domain, domain )
    }


{-|
-}
type alias ChannelWithLegend data range =
    { title : Maybe String
    , isAggregate : Bool
    , isVector : Bool
    , fieldEqual : data -> data -> Bool
    , fieldCompare : data -> data -> Order
    , legend : Int -> Legend range
    , displayLegend : Bool
    , extract : data -> Maybe range
    , extractVector : List data -> List (Maybe range)
    , summarize : List data -> Maybe range
    }


extract : ChannelWithLegend data range -> (data -> Maybe range)
extract { extract } =
    extract


extractVector : ChannelWithLegend data range -> (List data -> List (Maybe range))
extractVector { extractVector } =
    extractVector


summarize : ChannelWithLegend data range -> (List data -> Maybe range)
summarize { summarize } =
    summarize


isVector : { b | isVector : a } -> a
isVector { isVector } =
    isVector


isAggregate : { b | isAggregate : a } -> a
isAggregate { isAggregate } =
    isAggregate


compareMaybeAt : Maybe { a | fieldCompare : data -> data -> Order } -> data -> data -> Order
compareMaybeAt channel d1 d2 =
    Maybe.maybe
        EQ
        (\ch -> ch.fieldCompare d1 d2)
        channel


compareAt : { a | fieldCompare : data -> data -> Order } -> data -> data -> Order
compareAt channel d1 d2 =
    channel.fieldCompare d1 d2


equalAt : { a | fieldEqual : data -> data -> Bool } -> data -> data -> Bool
equalAt channel d1 d2 =
    channel.fieldEqual d1 d2


equalMaybeAt : Maybe { a | fieldEqual : data -> data -> Bool } -> data -> data -> Bool
equalMaybeAt maybeChannel d1 d2 =
    Maybe.maybe True (\ch -> ch.fieldEqual d1 d2) maybeChannel


legend : Int -> ChannelWithLegend data a -> Maybe (Legend a)
legend ticks channel =
    let
        legend =
            channel.legend ticks
    in
        if Legend.isEmpty legend then
            Nothing
        else
            Just legend



-- Positional channels ---------------------------------------------------------


{-| -}
positional :
    Field data comparableDomain
    -> PositionalChannel data comparableDomain
positional field =
    positionalCompareWith compare field


{-| -}
positionalCompareWith :
    (domain -> domain -> Order)
    -> Field data domain
    -> PositionalChannel data domain
positionalCompareWith customCompare field =
    { title = Field.fieldName field
    , isAggregate = Field.isAggregate field
    , isVector = Field.isVector field
    , fieldEqual = Field.equalAt field
    , fieldCompare = Field.compareAt customCompare field
    , field = field
    , displayAxis = True
    , compareDomain = customCompare
    , limits = Nothing
    }



-- Channels with legends -------------------------------------------------------


channelCompareWith :
    (domain -> domain -> Order)
    -> (domain -> String)
    -> Scale domain range
    -> Field data domain
    -> ChannelWithLegend data range
channelCompareWith compareWith formatDomain scale field =
    { title = Field.fieldName field
    , isAggregate = Field.isAggregate field
    , isVector = Field.isVector field
    , fieldEqual = Field.equalAt field
    , fieldCompare = Field.compareAt compareWith field
    , legend = Legend.legend formatDomain field scale
    , displayLegend = True
    , extract = Field.extract field >> Maybe.andThen (Scale.scale scale)
    , extractVector = Field.extractVector field >> List.map (Maybe.andThen (Scale.scale scale))
    , summarize = Field.summarize field >> Maybe.andThen (Scale.scale scale)
    }


channel :
    (comparableDomain -> String)
    -> Scale comparableDomain range
    -> Field data comparableDomain
    -> ChannelWithLegend data range
channel formatDomain scale field =
    channelCompareWith compare formatDomain scale field



-- Angle ------------------------------------------------------------------------


type alias AngleChannel data =
    ChannelWithLegend data Float


angle :
    (comparableDomain -> String)
    -> Scale comparableDomain Float
    -> Field data comparableDomain
    -> AngleChannel data
angle formatDomain scale field =
    channel formatDomain scale field


angleCompareWith :
    (domain -> domain -> Order)
    -> (domain -> String)
    -> Scale domain Float
    -> Field data domain
    -> AngleChannel data
angleCompareWith compareWith formatDomain scale field =
    channelCompareWith compareWith formatDomain scale field



-- Color -----------------------------------------------------------------------


type alias ColorChannel data =
    ChannelWithLegend data Color


color :
    (comparableDomain -> String)
    -> Scale comparableDomain Color
    -> Field data comparableDomain
    -> ColorChannel data
color formatDomain scale field =
    channel formatDomain scale field


colorCompareWith :
    (domain -> domain -> Order)
    -> (domain -> String)
    -> Scale domain Color
    -> Field data domain
    -> ColorChannel data
colorCompareWith compareWith formatDomain scale field =
    channelCompareWith compareWith formatDomain scale field



-- Float ------------------------------------------------------------------------


type alias FloatChannel data =
    ChannelWithLegend data Float


float :
    (comparableDomain -> String)
    -> Scale comparableDomain Float
    -> Field data comparableDomain
    -> FloatChannel data
float formatDomain scale field =
    channel formatDomain scale field


floatCompareWith :
    (domain -> domain -> Order)
    -> (domain -> String)
    -> Scale domain Float
    -> Field data domain
    -> FloatChannel data
floatCompareWith compareWith formatDomain scale field =
    channelCompareWith compareWith formatDomain scale field



-- Float ------------------------------------------------------------------------


type alias IntChannel data =
    ChannelWithLegend data Int


int :
    (comparableDomain -> String)
    -> Scale comparableDomain Int
    -> Field data comparableDomain
    -> IntChannel data
int formatDomain scale field =
    channel formatDomain scale field


intCompareWith :
    (domain -> domain -> Order)
    -> (domain -> String)
    -> Scale domain Int
    -> Field data domain
    -> IntChannel data
intCompareWith compareWith formatDomain scale field =
    channelCompareWith compareWith formatDomain scale field



-- Shape ------------------------------------------------------------------------


type alias ShapeChannel data =
    ChannelWithLegend data Shape


shape :
    (comparableDomain -> String)
    -> Scale comparableDomain Shape
    -> Field data comparableDomain
    -> ShapeChannel data
shape formatDomain scale field =
    channel formatDomain scale field


shapeCompareWith :
    (domain -> domain -> Order)
    -> (domain -> String)
    -> Scale domain Shape
    -> Field data domain
    -> ShapeChannel data
shapeCompareWith compareWith formatDomain scale field =
    channelCompareWith compareWith formatDomain scale field



-- StrokeDash ------------------------------------------------------------------


type alias StrokeDashChannel data =
    ChannelWithLegend data StrokeDash


strokeDash :
    (comparableDomain -> String)
    -> Scale comparableDomain StrokeDash
    -> Field data comparableDomain
    -> StrokeDashChannel data
strokeDash formatDomain scale field =
    channel formatDomain scale field


strokeDashCompareWith :
    (domain -> domain -> Order)
    -> (domain -> String)
    -> Scale domain StrokeDash
    -> Field data domain
    -> StrokeDashChannel data
strokeDashCompareWith compareWith formatDomain scale field =
    channelCompareWith compareWith formatDomain scale field



-- Text ------------------------------------------------------------------------


type alias TextChannel data =
    ChannelWithLegend data String


text :
    (comparableDomain -> String)
    -> Scale comparableDomain String
    -> Field data comparableDomain
    -> TextChannel data
text formatDomain scale field =
    channel formatDomain scale field


textCompareWith :
    (domain -> domain -> Order)
    -> (domain -> String)
    -> Scale domain String
    -> Field data domain
    -> TextChannel data
textCompareWith compareWith formatDomain scale field =
    channelCompareWith compareWith formatDomain scale field
