{ pkgs ? import <nixpkgs> { } }:
let
  easy-ps = import (builtins.fetchGit {
    url = "git@github.com:justinwoo/easy-purescript-nix.git";
    ref = "master";
    rev = "dae91f43317fd5ff207e11ea6bf4b6130e4ba9fc";
  }) { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = [
    easy-ps.purs-0_14_1
    easy-ps.spago
    easy-ps.pulp
    pkgs.nodejs-14_x
    pkgs.nodePackages.bower
  ];
  LC_ALL = "C.UTF-8"; # https://github.com/purescript/spago/issues/507
}
