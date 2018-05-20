{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, containers
      , directory, GLUT, GLUtil, JuicyPixels, lens, linear, mtl, OpenGL
      , OpenGLRaw, stdenv, stm, vector
      }:
      mkDerivation {
        pname = "purespace";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base containers directory lens linear mtl stm vector
        ];
        executableHaskellDepends = [
          aeson base bytestring containers GLUT GLUtil JuicyPixels linear
          OpenGL OpenGLRaw stm vector
        ];
        testHaskellDepends = [ base ];
        homepage = "http://github.com/hussein-aitlahcen/purespace";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
