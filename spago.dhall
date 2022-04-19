{-
-}
{ name = "arraybuffer-builder"
, dependencies =
  [ "effect"
  , "float32"
  , "identity"
  , "lists"
  , "maybe"
  , "newtype"
  , "prelude"
  , "transformers"
  , "uint"
  , "arraybuffer-types"
  , "arraybuffer"
  , "tailrec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/jamesdbrock/purescript-arraybuffer-builder"
}
