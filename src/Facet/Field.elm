module Facet.Field
    exposing
        ( Field
        , constant
        , scalar
        , maybeScalar
        , aggregate
        , maybeAggregate
        , vector
        , maybeVector
        )

{-|
@docs Field

@docs constant

@docs scalar, maybeScalar

@docs aggregate, maybeAggregate

@docs vector, maybeVector
-}

import Facet.Internal.Field as Field


{-| A `Field` is a means of extracting a value from some data type.

    There are different `Field`s allowing you to extract different 'shapes'
    of data:
    - A scalar `Field` extracts single item from a single piece of data;
    - A vector `Field` extracts a list of items from a list of data;
    - An aggregate `Field` summarizes a list of data as a single item.

    In addition, each type of field supports situations where the item you
    are extracting may be missing.
-}
type alias Field data domain =
    Field.Field data domain


{-| -}
constant : domain -> Field data domain
constant value =
    Field.constant value


{-| -}
scalar : { name : Maybe String, extract : data -> domain } -> Field data domain
scalar { name, extract } =
    Field.scalar name extract


{-| -}
maybeScalar : { a | name : Maybe String, extract : data -> Maybe domain } -> Field data domain
maybeScalar { name, extract } =
    Field.maybeScalar name extract


{-| -}
maybeVector : { a | name : Maybe String, extract : List data -> List (Maybe domain) } -> Field data domain
maybeVector { name, extract } =
    Field.maybeVector name extract


{-| -}
vector : { a | name : Maybe String, extract : List data -> List domain } -> Field data domain
vector { name, extract } =
    Field.vector name extract


{-| -}
maybeAggregate : { a | name : Maybe String, extract : List data -> Maybe domain } -> Field data domain
maybeAggregate { name, extract } =
    Field.maybeAggregate name extract


{-| -}
aggregate : { a | name : Maybe String, extract : List data -> domain } -> Field data domain
aggregate { name, extract } =
    Field.aggregate name extract
