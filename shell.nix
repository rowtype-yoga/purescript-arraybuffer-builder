{ pkgs ? import <nixpkgs> { } }:
let
  # easy-ps = import
  #   (pkgs.fetchFromGitHub {
  #     owner = "justinwoo";
  #     repo = "easy-purescript-nix";
  #     rev = "1ec689df0adf8e8ada7fcfcb513876307ea34226";
  #     sha256 = "12hk2zbjkrq2i5fs6xb3x254lnhm9fzkcxph0a7ngxyzfykvf4hi";
  #   }) {
  #   inherit pkgs;
  # };
  easy-ps = import (builtins.fetchGit {
    url = "git@github.com:ptrfrncsmrph/easy-purescript-nix.git";
    rev = "d53b10391c3ec289f8afdc664f743824115bbe70";
  }) { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = [
    easy-ps.purs-0_14_0-rc5
    easy-ps.spago
    easy-ps.pulp
    pkgs.nodejs-14_x
    pkgs.nodePackages.bower
  ];
  LC_ALL = "C.UTF-8"; # https://github.com/purescript/spago/issues/507
}
