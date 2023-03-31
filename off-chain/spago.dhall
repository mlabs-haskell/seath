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
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "monad-logger"
  , "mote"
  , "newtype"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "ordered-collections"
  , "parallel"
  , "payload"
  , "posix-types"
  , "prelude"
  , "profunctor-lenses"
  , "queue"
  , "quickcheck"
  , "refs"
  , "spec"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "uint"
  , "undefined"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
