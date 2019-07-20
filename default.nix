{ mkDerivation, async, base, brick, containers
, criterion-measurement, microlens, microlens-platform
, microlens-th, pretty-show, reactive-banana, stdenv, stm, text
, text-zipper, vector, vty
}:
mkDerivation {
  pname = "horca";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base brick containers criterion-measurement microlens
    microlens-platform microlens-th pretty-show reactive-banana stm
    text text-zipper vector vty
  ];
  executableHaskellDepends = [
    async base brick criterion-measurement microlens microlens-th
    reactive-banana stm text text-zipper vty
  ];
  license = stdenv.lib.licenses.bsd3;
}
