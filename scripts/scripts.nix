{ mkDerivation, base, bytestring, containers, either, filepath
, finite-typelits, megaparsec, optparse-applicative, raw-strings-qq
, stdenv, text, typed-process, utf8-string, vector, vector-sized
}:
mkDerivation {
  pname = "scripts";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers either filepath finite-typelits
    megaparsec optparse-applicative raw-strings-qq text typed-process
    utf8-string vector vector-sized
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
