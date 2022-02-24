{ mkDerivation, base, containers, directory, lib, network, unix }:
mkDerivation {
  pname = "quadratically";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers directory network unix
  ];
  license = lib.licenses.bsd3;
}
