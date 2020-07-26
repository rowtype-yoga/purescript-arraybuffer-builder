{-
-}
{ name = "arraybuffer-builder"
, dependencies =
  [ "arraybuffer-types"
  , "arraybuffer"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/jamesdbrock/purescript-arraybuffer-builder"
}
