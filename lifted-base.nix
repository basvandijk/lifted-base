{ mkDerivation, base, HUnit, monad-control, stdenv, test-framework
, test-framework-hunit, transformers, transformers-base
, transformers-compat, criterion, monad-peel
}:
mkDerivation {
  pname = "lifted-base";
  version = "HEAD";
  src = ./.;
  libraryHaskellDepends = [ base monad-control transformers-base ];
  testHaskellDepends = [
    base HUnit monad-control test-framework test-framework-hunit
    transformers transformers-base transformers-compat

    # Benchmark dependencies
    criterion monad-peel
  ];
  homepage = "https://github.com/basvandijk/lifted-base";
  description = "lifted IO operations from the base library";
  license = stdenv.lib.licenses.bsd3;
}
