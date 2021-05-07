{ pkgs ? import <nixpkgs> { } }:
let
  easy-ps = import (builtins.fetchGit {
    url = "https://github.com/justinwoo/easy-purescript-nix.git";
    ref = "master";
    rev = "dae91f43317fd5ff207e11ea6bf4b6130e4ba9fc";
  }) { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = [
    easy-ps.purs-0_14_1
    easy-ps.spago
    easy-ps.pulp
    easy-ps.psc-package
    pkgs.nodejs-14_x
    pkgs.nodePackages.bower
  ];
  LC_ALL = "C.UTF-8"; # https://github.com/purescript/spago/issues/507
  # https://github.com/purescript/spago#install-autocompletions-for-bash
  shellHook = ''
    source <(spago --bash-completion-script `which spago`)
  '';
}
