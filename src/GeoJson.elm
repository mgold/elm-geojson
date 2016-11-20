module GeoJson exposing (Bbox, FeatureObject, GeoJson, GeoJsonObject(..), Geometry(..), Position, decoder, encode)

{-| Decode [GeoJson](https://tools.ietf.org/html/rfc7946) into an Elm data
structure where you can operate on it further. Most of this module defines types
that collectively define that data structure.

After using `GeoJson.decoder` you can either traverse the data structure
directly  or use `Json.Decode.andThen` to transform it into a more convenient
representation specific to your use case. It is recommended that you try the
first approach first, and switch to the second if you encounter difficulty.

An `encode` function is also provided, mostly for completeness and testing.
Neither encoding nor decoding attempt to enforce minimum array lengths.

# Decoder
@docs decoder

# Elm Representation of GeoJSON
All union types are fully exposed so you can inspect them as necessary.
@docs GeoJson, GeoJsonObject, FeatureObject, Geometry, Position,  Bbox

# Encoding
@docs encode
-}

import Json.Encode as Json
import Json.Decode as D exposing (Decoder, field)


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

Note that the tag for `FeatureObject` is just `Feature`, to avoid a name
collision.
-}
type GeoJsonObject
    = Geometry Geometry
    | Feature FeatureObject
    | FeatureCollection (List FeatureObject)


{-| A `FeatureObject` represents a geographic feature. The `geometry` field is
allowed to have `null` instead of actual geometry, which is represented as
`Nothing`. The `properties` may be any JSON object but no attempt is made to
inspect it. The `id` is an optional "commonly used identifier". It is permitted
by the RFC to be either a string or a number; if the latter this implementation
converts it to a string.
-}
type alias FeatureObject =
    { geometry : Maybe Geometry
    , properties : Json.Value
    , id : Maybe String
    }


{-| The heart of GeoJSON: geometry objects. The union tags reflect the
`type` field of the JSON, and carries the value of the `coordinates` field (or
`geometries` for `GeometryCollection`).

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


{-| A `Position` is the fundamental geometry construct, and are represented in
JSON as an array of numbers. RFC 7946 states that "[t]he first two elements are
longitude and latitude, or easting and northing, precisely in that order". The
third element is the altitude. If omitted in the JSON, it will be set to zero.

As recommended by the RFC, position arrays with more than three elements are
rejected.
-}
type alias Position =
    ( Float, Float, Float )


{-| Decode GeoJSON into Elm. The decoded value is expressed in the types defined
by this module.
-}
decoder : Decoder GeoJson
decoder =
    D.map2 (,)
        ((field "type" D.string)
            |> D.andThen decodeGeoJson
        )
        (D.maybe (field "bbox" decodeBbox))


decodeGeoJson : String -> Decoder GeoJsonObject
decodeGeoJson tipe =
    let
        helper tipe =
            case tipe of
                "Feature" ->
                    D.map Feature decodeFeature

                "FeatureCollection" ->
                    D.map FeatureCollection (field "features" (D.list decodeFeature))

                _ ->
                    D.map Geometry decodeGeometry
    in
        (field "type" D.string)
            |> D.andThen helper


decodeFeature : Decoder FeatureObject
decodeFeature =
    D.map3 FeatureObject
        (field "geometry"
            (D.oneOf
                [ D.null Nothing
                , decodeGeometry |> D.map Just
                ]
            )
        )
        (field "properties" D.value)
        (D.maybe (field "id" (D.oneOf [ D.string, D.map toString D.int ])))


decodeGeometry : Decoder Geometry
decodeGeometry =
    let
        helper tipe =
            case tipe of
                "Point" ->
                    (field "coordinates" decodePosition)
                        |> D.map Point

                "MultiPoint" ->
                    (field "coordinates" (D.list decodePosition))
                        |> D.map MultiPoint

                "LineString" ->
                    (field "coordinates" (D.list decodePosition))
                        |> D.map LineString

                "MultiLineString" ->
                    (field "coordinates" (D.list (D.list decodePosition)))
                        |> D.map MultiLineString

                "Polygon" ->
                    (field "coordinates" (D.list (D.list decodePosition)))
                        |> D.map Polygon

                "MultiPolygon" ->
                    (field "coordinates" (D.list (D.list (D.list decodePosition))))
                        |> D.map MultiPolygon

                "GeometryCollection" ->
                    (field "geometries" (D.list decodeGeometry))
                        |> D.map GeometryCollection

                _ ->
                    D.fail <| "Unrecognized 'type': " ++ tipe
    in
        (field "type" D.string)
            |> D.andThen helper


decodeBbox : Decoder Bbox
decodeBbox =
    D.list D.float


decodePosition : Decoder Position
decodePosition =
    let
        errorString adj =
            "Array has too " ++ adj ++ " numbers to make a position"

        listToTuple ps =
            case ps of
                [] ->
                    D.fail (errorString "few")

                [ _ ] ->
                    D.fail (errorString "few")

                [ p1, p2 ] ->
                    D.succeed ( p1, p2, 0 )

                [ p1, p2, p3 ] ->
                    D.succeed ( p1, p2, p3 )

                _ ->
                    D.fail (errorString "many")
    in
        D.list D.float |> D.andThen listToTuple


{-| Encode GeoJSON into Elm. This is mostly for completeness and roundtrip
testing.

Positions with an altitude of zero will be encoded as two-element arrays.
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
encodePosition ( a, b, c ) =
    let
        coordinates =
            if c == 0 then
                [ a, b ]
            else
                [ a, b, c ]
    in
        coordinates
            |> List.map Json.float
            |> Json.list
