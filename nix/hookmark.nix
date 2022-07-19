{ mkDerivation, base, bytestring, containers, cookie, Diff
, directory, either, fetchgit, file-embed, filepath, hspec
, hspec-contrib, hspec-expectations, http-types, HUnit, lib
, megaparsec, nicify-lib, non-empty-text, optparse-applicative
, process, QuickCheck, raw-strings-qq, shakespeare
, template-haskell, temporary, text, typed-process, unix, unliftio
, utf8-string, yesod, yesod-form, yesod-static
}:
mkDerivation {
  pname = "hookmark";
  version = "1.4";
  src = fetchgit {
    url = "https://github.com/neosimsim/hookmark.git";
    sha256 = "0gqmhkm0scx1j3n1qdw6xivhpjjq4x1mmayqcgabr16dfimc41bw";
    rev = "df940020cb8a06944dc7ddda03f90baee360ceee";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers cookie directory either filepath
    http-types megaparsec non-empty-text process shakespeare
    template-haskell text typed-process utf8-string yesod yesod-form
    yesod-static
  ];
  executableHaskellDepends = [
    base bytestring directory filepath non-empty-text
    optparse-applicative raw-strings-qq template-haskell temporary text
    typed-process yesod
  ];
  testHaskellDepends = [
    base bytestring Diff directory either file-embed filepath hspec
    hspec-contrib hspec-expectations HUnit megaparsec nicify-lib
    non-empty-text process QuickCheck template-haskell text
    typed-process unix unliftio utf8-string
  ];
  homepage = "https://gitlab.com/neosimsim/hookmark";
  description = "Browser independent bookmark manager";
  license = lib.licenses.bsd3;
}
