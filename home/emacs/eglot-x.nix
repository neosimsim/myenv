{ trivialBuild
, fetchFromGitHub
, rust-mode
, eglot-x
}:
trivialBuild rec {
  pname = "eglot-x";
  version = "0.6";
  src = eglot-x;
}
