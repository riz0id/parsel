{ mkDerivation, base, deepseq, fetchgit, ghc-prim, hedgehog, lib
, prim-bool, prim-char, prim-int, prim-ord, tasty, tasty-hedgehog
, template-haskell
}:
mkDerivation {
  pname = "source-locations";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/source-locations";
    sha256 = "1ljjihgkz4dyhancsf3gslb5ihyk9pxrm3rh0q34ijj86fqfxnax";
    rev = "8ce7263b399db3b097170caccb7edb57ab589b47";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base deepseq ghc-prim prim-bool prim-char prim-int prim-ord
    template-haskell
  ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/source-locations";
  description = "Create and manipulate source file locations";
  license = lib.licenses.isc;
}
