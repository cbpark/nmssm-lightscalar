{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "nmssm-lightscalar";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  description = "For studies on the light scalar in the NMSSM";
  license = stdenv.lib.licenses.gpl3;
}
