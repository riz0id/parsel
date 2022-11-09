{ ghc ? "ghc924" }:

let 
  pkgs = import ../../default.nix { 
    inherit ghc; 
  };
in pkgs.parse-lisp.env.overrideAttrs (self: {
  buildInputs = self.buildInputs ++ [ 
  ];
})
