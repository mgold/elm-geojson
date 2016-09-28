module GeoJson exposing (Bbox, FeatureObject, GeoJson, GeoJsonObject(..), Geometry(..), Position, decoder, encode)

{-| Decode GeoJson into an Elm data structure where you can operate on it
further.

After using `GeoJson.decoder` you can either traverse the data structure
directly (recommended if you're working with 2D positions and not using
properties) or use `Json.Decode.andThen` to transform it into a more convenient
representation (recommended if you find yourself with a lot of Maybes or
impossible cases using the first approach). Also, GeoJSON objects may contain
nonstandard top-level fields; you can run multiple decoders using
`Json.Decode.object2` the way you'd use `map2`.

An `encode` function is also provided, mostly for completeness and testing.
Neither encoding nor decoding attempt to enforce minimum array lengths
(excluding positions themselves).

# Decoder
@docs decoder

# Elm Representation of GeoJSON
All union types are fully exposed so you can inspect them as necessary.
@docs GeoJson, GeoJsonObject, FeatureObject, Geometry, Position,  Bbox

# Encoding
@docs encode
-}

import Json.Encode as Json
import Json.Decode as D exposing (Decoder, (:=))


{-| A Bounding Box is represented as a simple list of floats. No attempt is made
to validate that its length is twice the length of the geometrys' positions, or
that low values are preceded by high values.
-}
type alias Bbox =
    List Float


{-| The root representation of GeoJSON in Elm. It consists of a `GeoJsonObject` and an optional `Bbox`.
-}
type alias GeoJson =
    ( GeoJsonObject, Maybe Bbox )


{-| A GeoJsonObject contains the primary data, and is either a `Geometry`, a
`FeatureObject`, or a list of `FeatureObjects`.
-}
type GeoJsonObject
    = Geometry Geometry
      -- Feature not FeatureObject to avoid name collision
    | Feature FeatureObject
    | FeatureCollection (List FeatureObject)


{-| A `FeatureObject` represents a geographic feature. The `geometry` field is
allowed to have `null` instead of actual geometry, which is represented as
`Nothing`. The `properties` may be any JSON object or `null` but no attempt
is made to inspect them. The `id` is an optional "commonly used identifier",
and has been taken to be a string even though the spec does not say so
explicitly.
-}
type alias FeatureObject =
    { geometry : Maybe Geometry
    , properties : Json.Value
    , id : Maybe String
    }


{-| At last, the heart of GeoJSON: geomtry objects. The union tags reflect the
`type` field of the JSON, and carries the value of the `coordinates` field (or
`geometries` for `GeometryCollection`.

The specification imposes minimum lengths for some of the arrays (lists in Elm).
This representation does not express those guarantees, on the theory that you
will likely be working with a valid GeoJson file rather than generating one of
your own.
-}
type Geometry
    = Point Position
    | MultiPoint (List Position)
    | LineString (List Position)
    | MultiLineString (List (List Position))
    | Polygon (List (List Position))
    | MultiPolygon (List (List (List Position)))
    | GeometryCollection (List Geometry)


{-| A `Position` is the fundamental geometry construct. The specification
represents positions as arrays of numbers that must contain at least two
elements. This tuple allows the compiler to provide a similar guarantee, so that
one avoids Maybes when working with a 2D dataset.
-}
type alias Position =
    ( Float, Float, List Float )


{-| Decode GeoJSON into Elm.
-}
decoder : Decoder GeoJson
decoder =
    D.object2 (,)
        (("type" := D.string) `D.andThen` decodeGeoJson)
        (D.maybe ("bbox" := decodeBbox))


decodeGeoJson : String -> Decoder GeoJsonObject
decodeGeoJson tipe =
    let
        helper tipe =
            case tipe of
                "Feature" ->
                    D.map Feature decodeFeature

                "FeatureCollection" ->
                    D.map FeatureCollection ("features" := (D.list decodeFeature))

                _ ->
                    D.map Geometry decodeGeometry
    in
        ("type" := D.string) `D.andThen` helper


decodeFeature : Decoder FeatureObject
decodeFeature =
    D.object3 FeatureObject
        ("geometry"
            := D.oneOf
                [ D.null Nothing
                , decodeGeometry |> D.map Just
                ]
        )
        ("properties" := D.value)
        (D.maybe ("id" := D.string))


decodeGeometry : Decoder Geometry
decodeGeometry =
    let
        helper tipe =
            case tipe of
                "Point" ->
                    ("coordinates" := decodePosition)
                        |> D.map Point

                "MultiPoint" ->
                    ("coordinates" := D.list decodePosition)
                        |> D.map MultiPoint

                "LineString" ->
                    ("coordinates" := D.list decodePosition)
                        |> D.map LineString

                "MultiLineString" ->
                    ("coordinates" := D.list (D.list decodePosition))
                        |> D.map MultiLineString

                "Polygon" ->
                    ("coordinates" := D.list (D.list decodePosition))
                        |> D.map Polygon

                "MultiPolygon" ->
                    ("coordinates" := D.list (D.list (D.list decodePosition)))
                        |> D.map MultiPolygon

                "GeometryCollection" ->
                    ("geometries" := D.list decodeGeometry)
                        |> D.map GeometryCollection

                _ ->
                    D.fail <| "Unrecognized 'type': " ++ tipe
    in
        ("type" := D.string) `D.andThen` helper


decodeBbox : Decoder Bbox
decodeBbox =
    D.list D.float


decodePosition : Decoder Position
decodePosition =
    D.list D.float
        `D.andThen`
            (\ps ->
                case ps of
                    p1 :: p2 :: more ->
                        D.succeed ( p1, p2, more )

                    _ ->
                        D.fail "Array has too few numbers to make a position"
            )


{-| encode GeoJSON into Elm.
-}
encode : GeoJson -> Json.Value
encode ( geojson, bbox ) =
    Json.object <| encodeGeoJson geojson ++ encodeBbox bbox


encodeGeoJson : GeoJsonObject -> List ( String, Json.Value )
encodeGeoJson geojson =
    case geojson of
        Feature feature ->
            encodeFeature feature

        FeatureCollection features ->
            [ ( "type", Json.string "FeatureCollection" )
            , ( "features", features |> List.map (encodeFeature >> Json.object) |> Json.list )
            ]

        Geometry geometry ->
            encodeGeometry geometry


encodeFeature : FeatureObject -> List ( String, Json.Value )
encodeFeature { geometry, properties, id } =
    let
        encodedId =
            case id of
                Nothing ->
                    []

                Just theId ->
                    [ ( "id", Json.string theId ) ]
    in
        [ ( "type", Json.string "Feature" )
        , ( "geometry", geometry |> Maybe.map (encodeGeometry >> Json.object) |> Maybe.withDefault Json.null )
        , ( "properties", properties )
        ]
            ++ encodedId


encodeGeometry : Geometry -> List ( String, Json.Value )
encodeGeometry geom =
    case geom of
        Point data ->
            [ ( "type", Json.string "Point" )
            , ( "coordinates", encodePosition data )
            ]

        MultiPoint data ->
            [ ( "type", Json.string "MultiPoint" )
            , ( "coordinates", data |> List.map encodePosition |> Json.list )
            ]

        LineString data ->
            [ ( "type", Json.string "LineString" )
            , ( "coordinates", data |> List.map encodePosition |> Json.list )
            ]

        MultiLineString data ->
            [ ( "type", Json.string "MultiLineString" )
            , ( "coordinates", data |> List.map (List.map encodePosition >> Json.list) |> Json.list )
            ]

        Polygon data ->
            [ ( "type", Json.string "Polygon" )
            , ( "coordinates", data |> List.map (List.map encodePosition >> Json.list) |> Json.list )
            ]

        MultiPolygon data ->
            [ ( "type", Json.string "MultiPolygon" )
            , ( "coordinates", data |> List.map (List.map (List.map encodePosition >> Json.list) >> Json.list) |> Json.list )
            ]

        GeometryCollection data ->
            [ ( "type", Json.string "GeometryCollection" )
            , ( "geometries", data |> List.map (encodeGeometry >> Json.object) |> Json.list )
            ]


encodeBbox : Maybe Bbox -> List ( String, Json.Value )
encodeBbox bbox =
    bbox
        |> Maybe.map (\b -> [ ( "bbox", b |> List.map Json.float |> Json.list ) ])
        |> Maybe.withDefault []


encodePosition : Position -> Json.Value
encodePosition ( a, b, more ) =
    (a :: b :: more)
        |> List.map Json.float
        |> Json.list
