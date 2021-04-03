{ mkDerivation, alex, array, base, containers, happy, haskell-say
, hspec, HUnit, mtl, prettyprinter, prettyprinter-ansi-terminal
, raw-strings-qq, stdenv, text
}:
mkDerivation {
  pname = "hlambda";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base containers mtl prettyprinter prettyprinter-ansi-terminal
    text
  ];
  libraryToolDepends = [ alex happy ];
  executableHaskellDepends = [ base haskell-say ];
  testHaskellDepends = [ base hspec HUnit raw-strings-qq ];
  license = stdenv.lib.licenses.mit;
}
