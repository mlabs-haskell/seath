{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "seath"
, dependencies =
  [ "aff"
  , "arrays"
  , "bigints"
  , "cardano-transaction-lib"
  , "datetime"
  , "effect"
  , "exceptions"
  , "maybe"
  , "mote"
  , "newtype"
  , "ordered-collections"
  , "posix-types"
  , "prelude"
  , "profunctor-lenses"
  , "spec"
  , "transformers"
  , "tuples"
  , "uint"
  , "undefined"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
