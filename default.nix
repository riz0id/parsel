{ ghc ? "ghc922" }:

let
  pkgs = import nix/pkgs.nix { 
    inherit ghc;
  };
in {
  inherit (pkgs.haskell.packages."${ghc}") 
    fourmolu
    haskell-language-server
    hlint
    parsel-parse; 
    
  inherit (pkgs) 
    cabal-install 
    clang 
    llvm;
}

