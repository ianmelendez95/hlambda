let 
  pkgs = import <nixpkgs> {};
in 
  pkgs.haskellPackages.callPackage ./hlambda-cabal.nix {}
