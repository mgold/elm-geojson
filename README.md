# elm-geojson

```
elm install mgold/elm-geojson
```

A library to decode and encode geoJson into a format that's nice to use in Elm. Turn this:

```json
{ "type": "LineString",
  "coordinates": [ [100.0, 0.0], [101.0, 1.0] ],
  "bbox": [-10.0, -10.0, 10.0, 10.0]
}
```

into this:

```elm
(Geometry (LineString [ ( 100, 0, 0 ), ( 101, 1, 0 ) ]), Just [-10, -10, 10, 10])
```

For more details, see the GeoJson module docs. GeoJSON itself is defined in [RFC 7946](https://tools.ietf.org/html/rfc7946).


## Testing
```
npm install -g elm-test
elm test
```

The test suite includes a fuzz testing that causes testing to take anywhere from half a second to thirty seconds. Very occasionally, it may cause a JS out of memory error.

If you get an actual failing test, please file a bug that includes the seed to reproduce.
