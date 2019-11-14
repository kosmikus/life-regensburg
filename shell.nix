with (import (builtins.fetchGit {
    name = "nixos-unstable-2019-08-25";
    url = https://github.com/nixos/nixpkgs/;
    rev = "f188bad6eaf26ebee19d02df03b1c6ae56c4d7f6";
  }) {}).pkgs;

# with (import <nixpkgs> {}).pkgs;

let
  ghc = haskell.packages.ghc865.ghcWithPackages
    (pkgs : with pkgs; []);
in
  stdenv.mkDerivation {
    name = "dev-life";
    buildInputs = [ ghc pkgconfig libGL libGLU freeglut ];
    shellHook = ''
      eval $(grep export ${ghc}/bin/ghc)
      export LD_LIBRARY_PATH="${libGL}/lib:${libGLU}/lib:${freeglut}/lib";
    '';
  }
