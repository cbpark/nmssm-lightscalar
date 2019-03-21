{ mkDerivation, base, bytestring, double-conversion, mwc-random
, optparse-generic, stdenv, transformers, vector
}:
mkDerivation {
  pname = "nmssm-lightscalar";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring double-conversion mwc-random optparse-generic
    transformers vector
  ];
  description = "For studies on the light scalar in the NMSSM";
  license = stdenv.lib.licenses.gpl3;
}
