module GeoJson exposing (Bbox, Crs(..), FeatureObject, GeoJson, GeoJsonObject(..), Geometry(..), Position, decode)

{-| Module docs here.
@docs decode,Bbox, Crs, FeatureObject, GeoJson, GeoJsonObject, Geometry, Position
-}

import Json.Encode as Json
import Json.Decode as D exposing (Decoder, (:=))


{-| A Coordinate Reference System may be either JSON `null`, a name, or a link
containing an href and optionally a type. No attempt is made to validate that
the href is a "dereferenceable URI".
-}
type Crs
    = Null
    | Name String
    | Link String (Maybe String)


{-| A Bounding Box is represented as a simple list of floats. No attempt is made
to validate that its length is twice the length of the geometrys' positions, or
that low values are preceded by high values.
-}
type alias Bbox =
    List Float


{-| The root representation of GeoJSON in Elm. It consists of a `GeoJsonObject`, and optional `Crs`, and an optional `Bbox`. Note that a `myGeoJson.crs == Nothing` means that no CRS was present in the JSON, and `myGeoJson.crs == Just Null` means that it was `null`, and therefore "no CRS can be assumed".
-}
type alias GeoJson =
    ( GeoJsonObject, Maybe Crs, Maybe Bbox )


{-| A GeoJsonObject contains the primary data, and is either a `Geometry`, a
`FeatureObject`, or a list of `FeatureObjects`. Note the prime on
`FeatureObject'` to prevent a collision with the `FeatureObject` record
constructor.
-}
type GeoJsonObject
    = Geometry Geometry
    | FeatureObject' FeatureObject
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
    | MutliPolygon (List (List (List Position)))
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
decode : Decoder GeoJson
decode =
    D.object3 (,,) decodeGeoJson (D.maybe decodeCrs) (D.maybe decodeBbox)


decodeGeoJson : Decoder GeoJsonObject
decodeGeoJson =
    Debug.crash "TODO"


decodeCrs : Decoder Crs
decodeCrs =
    D.oneOf
        [ D.null Null
        , ("type" := D.string)
            `D.andThen`
                (\tipe ->
                    if tipe == "name" then
                        D.object1 Name
                            (D.at [ "properties", "name" ] D.string)
                    else if tipe == "link" then
                        D.object2 Link
                            (D.at [ "properties", "href" ] D.string)
                            (D.at [ "properties", "type" ] D.string |> D.maybe)
                    else
                        D.fail <| "Unrecognized CRS type: " ++ tipe
                )
        ]


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
