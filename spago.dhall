{-
-}
{ name = "arraybuffer-builder"
, dependencies =
  [ "effect"
  , "float32"
  , "maybe"
  , "prelude"
  , "transformers"
  , "uint"
  , "arraybuffer-types"
  , "arraybuffer"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/jamesdbrock/purescript-arraybuffer-builder"
}
