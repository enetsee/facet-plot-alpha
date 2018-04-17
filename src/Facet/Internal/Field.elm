module Facet.Internal.Field
    exposing
        ( Field
        , constant
        , scalar
        , maybeScalar
        , aggregate
        , maybeAggregate
        , vector
        , maybeVector
        , extract
        , extractVector
        , summarize
        , fieldName
        , isAggregate
        , isConstant
        , isScalar
        , isVector
        , equalAt
        , compareAt
        )


type Field data domain
    = Constant domain
    | Scalar (Maybe String) (data -> Maybe domain)
    | Vector (Maybe String) (List data -> List (Maybe domain))
    | Aggregate (Maybe String) (List data -> Maybe domain)


equalAt : Field data domain -> data -> data -> Bool
equalAt field d1 d2 =
    Maybe.withDefault True <|
        Maybe.map2 (==)
            (extract field d1)
            (extract field d2)


compareAt : (domain -> domain -> Order) -> Field data domain -> data -> data -> Order
compareAt compareWith field d1 d2 =
    Maybe.withDefault EQ <|
        Maybe.map2 (compareWith)
            (extract field d1)
            (extract field d2)


scalar : Maybe String -> (data -> domain) -> Field data domain
scalar name extract =
    Scalar name <| Just << extract


maybeScalar : Maybe String -> (data -> Maybe domain) -> Field data domain
maybeScalar name extract =
    Scalar name extract


maybeVector : Maybe String -> (List data -> List (Maybe domain)) -> Field data domain
maybeVector name extract =
    Vector name extract


vector : Maybe String -> (List data -> List domain) -> Field data domain
vector name extract =
    Vector name (List.map Just << extract)


maybeAggregate : Maybe String -> (List data -> Maybe domain) -> Field data domain
maybeAggregate name summarize =
    Aggregate name summarize


aggregate : Maybe String -> (List data -> domain) -> Field data domain
aggregate name summarize =
    Aggregate name <| Just << summarize


constant : domain -> Field data domain
constant value =
    Constant value


extract : Field data domain -> data -> Maybe domain
extract field datum =
    case field of
        Constant value ->
            Just value

        Scalar _ extract ->
            extract datum

        _ ->
            Nothing


extractVector : Field data domain -> List data -> List (Maybe domain)
extractVector field data =
    case field of
        Constant value ->
            [ Just value ]

        Vector _ extract ->
            extract data

        _ ->
            []


summarize : Field data domain -> List data -> Maybe domain
summarize field data =
    case field of
        Constant value ->
            Just value

        Aggregate _ summarize ->
            summarize data

        _ ->
            Nothing


fieldName : Field data domain -> Maybe String
fieldName field =
    case field of
        Constant _ ->
            Nothing

        Scalar name _ ->
            name

        Vector name _ ->
            name

        Aggregate name _ ->
            name


isScalar : Field data domain -> Bool
isScalar field =
    case field of
        Scalar _ _ ->
            True

        _ ->
            False


isVector : Field data domain -> Bool
isVector field =
    case field of
        Vector _ _ ->
            True

        _ ->
            False


isConstant : Field data domain -> Bool
isConstant field =
    case field of
        Constant _ ->
            True

        _ ->
            False


isAggregate : Field data domain -> Bool
isAggregate field =
    case field of
        Aggregate _ _ ->
            True

        _ ->
            False
