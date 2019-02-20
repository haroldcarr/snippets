{ mkDerivation, base, stdenv, ... }@args:
mkDerivation {
  pname = "nix-assert-bug";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ args."assert" base ];
  license = stdenv.lib.licenses.bsd3;
}
