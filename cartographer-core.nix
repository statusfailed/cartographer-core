{ mkDerivation, base, bimap, containers, criterion, deepseq, logict
, QuickCheck, reflection, stdenv, tasty, tasty-quickcheck, time
}:
mkDerivation {
  pname = "cartographer-core";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bimap containers deepseq logict reflection
  ];
  executableHaskellDepends = [
    base bimap containers criterion deepseq logict time
  ];
  testHaskellDepends = [
    base bimap containers logict QuickCheck tasty tasty-quickcheck time
  ];
  license = stdenv.lib.licenses.mit;
}
