{ mkDerivation, base, mwc-random, parallel, stdenv, transformers
, vector
}:
mkDerivation {
  pname = "nmssm-lightscalar";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base mwc-random parallel transformers vector
  ];
  description = "For studies on the light scalar in the NMSSM";
  license = stdenv.lib.licenses.gpl3;
}
