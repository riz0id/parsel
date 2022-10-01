{ mkDerivation, base, fetchgit, ghc-prim, lib, prim-bool
, prim-compat, template-haskell
}:
mkDerivation {
  pname = "prim-ord";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-ord";
    sha256 = "1d1nrvpgbhf6n4vlhfjdfsm839ra0149w9cw7cfzsp61jw1ay5iw";
    rev = "92bfe3a2b12c5caef28fc4ea398a6de8a6423841";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim prim-bool prim-compat template-haskell
  ];
  homepage = "https://github.com/riz0id/prim-ord";
  description = "TODO";
  license = lib.licenses.isc;
}
