{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  project = import ./release.nix;

in
  pkgs.stdenv.mkDerivation {
#    shellHook = ''
#alias nix-shell=""
#'';
    name = "haskell-shell";
    buildInputs = project.env.nativeBuildInputs ++ [
      pkgs.busybox
      pkgs.man
      pkgs.nix
      pkgs.ghc
      haskellPackages.hlint
      haskellPackages.cabal2nix
      haskellPackages.cabal-install
      haskellPackages.hoogle
  ];
}
