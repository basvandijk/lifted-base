{ cabal, HUnit, monadControl, testFramework, testFrameworkHunit
, transformers, transformersBase, transformersCompat
}:

cabal.mkDerivation (self: {
  pname = "lifted-base";
  version = "HEAD";
  src = ./.;
  buildDepends = [ monadControl transformersBase ];
  testDepends = [
    HUnit monadControl testFramework testFrameworkHunit transformers
    transformersBase transformersCompat
  ];
  meta = {
    homepage = "https://github.com/basvandijk/lifted-base";
    description = "lifted IO operations from the base library";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
