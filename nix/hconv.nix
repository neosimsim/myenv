{ mkDerivation
, base
, fetchgit
, lib
, optparse-applicative
, text
, unicode-transforms
}:
mkDerivation {
  pname = "hconv";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/neosimsim/hconv.git";
    sha256 = "11wz8a3iq1x81kx7gw06iacdza8nvcdph3zb53lxmlsczc8dwqaq";
    rev = "05b254dc4e2c9258f7d9a4721847376a001b99de";
    fetchSubmodules = true;
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    optparse-applicative
    text
    unicode-transforms
  ];
  description = "cli tool to normalize unicode input";
  license = lib.licenses.bsd3;
}
