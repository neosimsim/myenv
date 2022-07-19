{ mkDerivation, base, bytestring, lib, text, typed-process }:
mkDerivation {
  pname = "neosimsim-shell";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring text typed-process ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
