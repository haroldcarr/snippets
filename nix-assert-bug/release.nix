let pkgs = import <nixpkgs> { };
in pkgs.haskellPackages.callPackage ./default.nix { "assert" = pkgs.haskellPackages."assert"; }
