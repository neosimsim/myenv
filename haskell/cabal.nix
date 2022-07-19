{ mkDerivation, base, bytestring, containers, either, filepath
, finite-typelits, Glob, hspec, interpolate, lib, megaparsec
, mono-traversable, optparse-applicative, QuickCheck
, raw-strings-qq, regex-tdfa, safe, text, typed-process
, utf8-string, vector, vector-sized
}:
mkDerivation {
  pname = "scripts";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base filepath Glob interpolate mono-traversable raw-strings-qq
    regex-tdfa safe text
  ];
  executableHaskellDepends = [
    base bytestring containers either filepath finite-typelits
    megaparsec optparse-applicative raw-strings-qq text typed-process
    utf8-string vector vector-sized
  ];
  testHaskellDepends = [
    base hspec interpolate QuickCheck regex-tdfa safe text
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
