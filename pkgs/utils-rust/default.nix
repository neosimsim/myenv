{ rustPlatform }:
rustPlatform.buildRustPackage {
  pname = "utils";
  version = "0.1.0";
  src = ./.;
  cargoLock = {
    lockFile = ./Cargo.lock;
  };
}
