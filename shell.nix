{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, directory, GLUT
      , GLUtil, JuicyPixels, lens, mtl, OpenGL, OpenGLRaw, stdenv, stm
      , vector
      }:
      mkDerivation {
        pname = "purespace";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base bytestring directory lens mtl stm vector
        ];
        executableHaskellDepends = [
          base GLUT GLUtil JuicyPixels OpenGL OpenGLRaw
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
