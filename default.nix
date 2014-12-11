let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callPackage ./lifted-base.nix {}
