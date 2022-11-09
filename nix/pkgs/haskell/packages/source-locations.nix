{ mkDerivation, base, deepseq, fetchgit, ghc-prim, hedgehog, lib
, prim-bool, prim-char, prim-int, prim-ord, tasty, tasty-hedgehog
, template-haskell, text
}:
mkDerivation {
  pname = "source-locations";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/source-locations";
    sha256 = "1xgbyzpfzar52k4376mwv7xqcipl8im8xc1823a1y5g43ali0bah";
    rev = "c6f7fc05ae11094bb954e66c5981061fa6ac9885";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base deepseq ghc-prim prim-bool prim-char prim-int prim-ord
    template-haskell text
  ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/source-locations";
  description = "Create and manipulate source file locations";
  license = lib.licenses.isc;
}
