# elm-geojson
**Status**: Experimental, not yet released to elm-package. Based on the original spec and not the RFC (TODO: fix that). But please try it (copy the source file) and report back!

A library to decode (and encode) geoJson into a format that's nice to use in Elm.

For docs, see src/geoJson.elm

## Testing
```
npm install -g elm-test
elm test
```

The tests take anywhere from 10 to 90 seconds to run on my laptop (thanks to highly-variable random testing).

If you get a test failure on master, it will indicate the seed in the output. Please report a bug and include that seed.

-------

(this doesn't work yet, not published)
```
elm package install -y mgold/elm-geojson
```


