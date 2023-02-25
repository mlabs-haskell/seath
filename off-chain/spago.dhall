{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "seath"
, dependencies =
  [ "aff"
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
  , "spec"
  , "uint"
  , "undefined"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
