{ mkDerivation, async, base, brick, containers, microlens
, microlens-platform, microlens-th, monoid-subclasses
, reactive-banana, stdenv, stm, text, text-zipper, vector, vty
}:
mkDerivation {
  pname = "horca";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base containers microlens microlens-platform microlens-th stm
    text text-zipper vector
  ];
  executableHaskellDepends = [
    async base brick microlens microlens-th monoid-subclasses
    reactive-banana stm text text-zipper vector vty
  ];
  license = stdenv.lib.licenses.bsd3;
}
