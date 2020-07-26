{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-easy-nix-example"
, dependencies =
  [ "arraybuffer-types"
  , "arraybuffer"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
