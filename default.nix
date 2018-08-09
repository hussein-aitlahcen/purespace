{ mkDerivation, aeson, base, bytestring, clock, containers
, directory, free, GLUT, GLUtil, JuicyPixels, lens, linear, mtl
, OpenGL, OpenGLRaw, pqueue, recursion-schemes, stdenv, stm, vector
}:
mkDerivation {
  pname = "purespace";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base clock containers directory free lens linear mtl pqueue
    recursion-schemes stm vector
  ];
  executableHaskellDepends = [
    aeson base bytestring clock containers GLUT GLUtil JuicyPixels
    linear OpenGL OpenGLRaw pqueue stm vector
  ];
  testHaskellDepends = [ base ];
  homepage = "http://github.com/hussein-aitlahcen/purespace";
  license = stdenv.lib.licenses.gpl3;
}
