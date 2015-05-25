## potrace

Mid-level bindings to the [potrace] library tracing a bitmap image to
vector paths. Contains helpers for converting [JuicyPixel] images to
bitmaps.

Uses [bindings-potrace] which is not yet on Hackage.

See [potrace-diagrams] for creating diagrams from bitmap images.

## potrace-bindings installation

### mac

With [homebrew]:

```
brew install potrace
```

`cabal` may complain about not finding the potrace library. In which can
you can add the `--extra-lib-dirs=/usr/local/lib
--extra-include-dirs=/usr/local/include` flags when `cabal install`ing.


[potrace]: http://potrace.sourceforge.net
[JuicyPixel]: https://github.com/Twinside/Juicy.Pixels
[bindings-potrace]: https://github.com/rwbarton/bindings-potrace
[potrace-diagrams]: https://github.com/cchalmers/potrace-diagrams
[homebrew]: http://brew.sh
