module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import GeoJson exposing (GeoJson, GeoJsonObject(..), Geometry(..), FeatureObject, Position, Crs(..), Bbox, decoder)
import Json.Decode exposing (decodeString)
import Json.Encode
import Fuzz exposing (Fuzzer)


all : Test
all =
    concat [ geometryExamples, expectedFailures, encodeAndDecode ]


encodeAndDecode : Test
encodeAndDecode =
    fuzzWith { runs = 50 } fuzzGeoJson "encoding and decoding does not change the GeoJson" <|
        \geojson ->
            GeoJson.encode geojson |> Json.Decode.decodeValue decoder |> Expect.equal (Ok geojson)


fuzzGeoJson : Fuzzer GeoJson
fuzzGeoJson =
    Fuzz.tuple3
        ( fuzzGeoJsonObject
        , (Fuzz.maybe fuzzCrs)
        , (Fuzz.maybe fuzzBbox)
        )


fuzzGeoJsonObject =
    Fuzz.frequencyOrCrash
        [ ( 1, Fuzz.map FeatureCollection (Fuzz.list fuzzFeature) )
        , ( 1, Fuzz.map Feature fuzzFeature )
        , ( 7, Fuzz.map Geometry fuzzGeometry )
        ]


fuzzFeature : Fuzzer FeatureObject
fuzzFeature =
    Fuzz.map3 FeatureObject
        (Fuzz.maybe fuzzGeometry)
        (Fuzz.constant (Json.Encode.object []))
        (Fuzz.maybe Fuzz.string)


fuzzGeometry : Fuzzer Geometry
fuzzGeometry =
    let
        helper depth =
            Fuzz.frequencyOrCrash
                [ ( 1, Fuzz.map Point fuzzPosition )
                , ( 1, Fuzz.map MultiPoint (Fuzz.list fuzzPosition) )
                , ( 1, Fuzz.map LineString (Fuzz.list fuzzPosition) )
                , ( 1, Fuzz.map MultiLineString (Fuzz.list (Fuzz.list fuzzPosition)) )
                , ( 1, Fuzz.map Polygon (Fuzz.list (Fuzz.list fuzzPosition)) )
                , ( 1, Fuzz.map (\xs -> MultiPolygon [ xs ]) (Fuzz.list (Fuzz.list fuzzPosition)) )
                , if depth > 3 then
                    ( 0, Fuzz.constant (Point ( 1, 2, [] )) )
                  else
                    ( 1 / depth, Fuzz.map GeometryCollection (Fuzz.list (helper (depth + 1))) )
                ]
    in
        helper 1


fuzzPosition : Fuzzer Position
fuzzPosition =
    Fuzz.map2 (\a b -> ( a, b, [] )) Fuzz.float Fuzz.float


fuzzCrs : Fuzzer Crs
fuzzCrs =
    Fuzz.frequencyOrCrash
        [ ( 1, Fuzz.constant Null )
        , ( 1, Fuzz.map Name Fuzz.string )
        , ( 1, Fuzz.map2 Link Fuzz.string (Fuzz.maybe Fuzz.string) )
        ]


fuzzBbox : Fuzzer Bbox
fuzzBbox =
    Fuzz.tuple4
        ( Fuzz.float
        , Fuzz.float
        , Fuzz.float
        , Fuzz.float
        )
        |> Fuzz.map (\( a, b, c, d ) -> [ a, b, c, d ])


expectErr : Result a b -> Expectation
expectErr r =
    case r of
        Ok _ ->
            Expect.fail <| "Expected an Err but got " ++ toString r

        Err _ ->
            Expect.pass


expectedFailures : Test
expectedFailures =
    describe "Invalid GeoJSON"
        [ test "Invalid type" <|
            \() ->
                let
                    json =
                        """{"type": "NotAnActualType"}"""
                in
                    decodeString decoder json |> expectErr
        , test "No coordinates" <|
            \() ->
                let
                    json =
                        """{"type": "Point"}"""
                in
                    decodeString decoder json |> expectErr
        , test "Not enough indices in position " <|
            \() ->
                let
                    json =
                        """{"type": "Point", "coordinates": [1]}"""
                in
                    decodeString decoder json |> expectErr
        ]


geometryExamples : Test
geometryExamples =
    let
        geomTest name { json, expected } =
            test name <|
                \() ->
                    decodeString decoder json
                        |> Expect.equal (Ok ( Geometry expected, Nothing, Nothing ))
    in
        describe "Geometry Examples from Appendix A of the specification"
            [ geomTest "Point"
                { json =
                    """{ "type": "Point", "coordinates": [100.0, 0.0] }"""
                , expected =
                    Point ( 100, 0, [] )
                }
            , geomTest "LineString"
                { json =
                    """
                       { "type": "LineString",
                        "coordinates": [ [100.0, 0.0], [101.0, 1.0] ]
                       }
                   """
                , expected =
                    LineString [ ( 100, 0, [] ), ( 101, 1, [] ) ]
                }
            , geomTest "Polygon"
                { json =
                    """
                    { "type": "Polygon",
                      "coordinates": [
                         [ [100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0], [100.0, 0.0] ]
                       ]
                    }
                   """
                , expected =
                    Polygon [ [ ( 100, 0, [] ), ( 101, 0, [] ), ( 101, 1, [] ), ( 100, 1, [] ), ( 100, 0, [] ) ] ]
                }
            , geomTest "Polygon with holes"
                { json =
                    """
                    { "type": "Polygon",
                      "coordinates": [
                         [ [100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0], [100.0, 0.0] ],
                         [ [100.2, 0.2], [100.8, 0.2], [100.8, 0.8], [100.2, 0.8], [100.2, 0.2] ]
                         ]
                      }
                    """
                , expected =
                    Polygon
                        [ [ ( 100, 0, [] ), ( 101, 0, [] ), ( 101, 1, [] ), ( 100, 1, [] ), ( 100, 0, [] ) ]
                        , [ ( 100.2, 0.2, [] ), ( 100.8, 0.2, [] ), ( 100.8, 0.8, [] ), ( 100.2, 0.8, [] ), ( 100.2, 0.2, [] ) ]
                        ]
                }
            , geomTest "MultiPoint"
                { json =
                    """
                    { "type": "MultiPoint",
                      "coordinates": [ [100.0, 0.0], [101.0, 1.0] ]
                    }
                    """
                , expected =
                    MultiPoint
                        [ ( 100, 0, [] ), ( 101, 1, [] ) ]
                }
            , geomTest "MultiLineString"
                { json =
                    """
                        { "type": "MultiLineString",
                          "coordinates": [
                            [ [100.0, 0.0], [101.0, 1.0] ],
                            [ [102.0, 2.0], [103.0, 3.0] ]
                          ]
                        }
                    """
                , expected =
                    MultiLineString
                        [ [ ( 100, 0, [] ), ( 101, 1, [] ) ]
                        , [ ( 102, 2, [] ), ( 103, 3, [] ) ]
                        ]
                }
            , geomTest "MultiPolygon"
                { json =
                    """
                        { "type": "MultiPolygon",
                          "coordinates": [
                              [[[102.0, 2.0], [103.0, 2.0], [103.0, 3.0], [102.0, 3.0], [102.0, 2.0]]],
                              [[[100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0], [100.0, 0.0]],
                               [[100.2, 0.2], [100.8, 0.2], [100.8, 0.8], [100.2, 0.8], [100.2, 0.2]]]
                              ]
                        }
                    """
                , expected =
                    MultiPolygon
                        [ [ [ ( 102, 2, [] ), ( 103, 2, [] ), ( 103, 3, [] ), ( 102, 3, [] ), ( 102, 2, [] ) ] ]
                        , [ [ ( 100, 0, [] ), ( 101, 0, [] ), ( 101, 1, [] ), ( 100, 1, [] ), ( 100, 0, [] ) ], [ ( 100.2, 0.2, [] ), ( 100.8, 0.2, [] ), ( 100.8, 0.8, [] ), ( 100.2, 0.8, [] ), ( 100.2, 0.2, [] ) ] ]
                        ]
                }
            , geomTest "GeometryCollection"
                { json =
                    """
                        { "type": "GeometryCollection",
                          "geometries": [
                            { "type": "Point",
                              "coordinates": [100.0, 0.0]
                            },
                            { "type": "LineString",
                              "coordinates": [ [101.0, 0.0], [102.0, 1.0] ]
                            }
                         ]
                      }
                    """
                , expected =
                    GeometryCollection
                        [ Point ( 100, 0, [] )
                        , LineString [ ( 101, 0, [] ), ( 102, 1, [] ) ]
                        ]
                }
            ]
