{ ghc ? "ghc924" }:

let
  pkgs = import nix/pkgs.nix { 
    inherit ghc;
  };
in {
  inherit (pkgs.haskell.packages."${ghc}") 
    parsel
    parse-lisp; 
    
  inherit (pkgs) 
    clang 
    llvm;
}

