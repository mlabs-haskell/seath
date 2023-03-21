{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "seath"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "bigints"
  , "cardano-transaction-lib"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "mote"
  , "newtype"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "ordered-collections"
  , "posix-types"
  , "prelude"
  , "profunctor-lenses"
  , "spec"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "uint"
  , "undefined"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
