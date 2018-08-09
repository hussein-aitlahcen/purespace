let
  compiler = "ghc822";
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = hpkgsNew: hpkgsOld: rec {
              purespace = hpkgsNew.callPackage ./default.nix {};
            };
          };
        };
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };
in
  pkgs.haskell.packages."${compiler}".purespace
