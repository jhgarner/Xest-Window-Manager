{ mkDerivation, base, base-prelude, bifunctors, chunked-data
, comonad, containers, deepseq, deriving-compat, dhall, directory
, free, freer-simple, generic-arbitrary, hpack, lens, mtl, process
, QuickCheck, quickcheck-instances, recursion-schemes, regex-compat
, sdl2, sdl2-ttf, semigroupoids, stdenv, template-haskell, text
, time, transformers, X11, gitignoreSource
}:
let
in mkDerivation {
  pname = "xest";
  version = "0.1.0.0";
  src = gitignoreSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base-prelude bifunctors chunked-data comonad containers
    deepseq deriving-compat dhall directory free freer-simple lens mtl
    process recursion-schemes regex-compat sdl2 sdl2-ttf semigroupoids
    template-haskell text time transformers X11
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base base-prelude bifunctors chunked-data comonad containers
    deepseq deriving-compat dhall directory free freer-simple lens mtl
    process recursion-schemes regex-compat sdl2 sdl2-ttf semigroupoids
    template-haskell text time transformers X11
  ];
  testHaskellDepends = [
    base base-prelude bifunctors chunked-data comonad containers
    deepseq deriving-compat dhall directory free freer-simple
    generic-arbitrary lens mtl process QuickCheck quickcheck-instances
    recursion-schemes regex-compat sdl2 sdl2-ttf semigroupoids
    template-haskell text time transformers X11
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/neXtWM#readme";
  license = stdenv.lib.licenses.bsd3;
}
