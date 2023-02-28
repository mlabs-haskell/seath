module Main (main) where

import AdditionValidator (additionScript)
import Serialize (toStringEnvelope)
import Prelude

main :: IO ()
main = do
  writeScriptTo (toStringEnvelope additionScript) "Addition"

writeScriptTo :: String -> String -> IO ()
writeScriptTo s name =
  -- TODO: https://github.com/mlabs-haskell/seath/issues/8
  let moduleHeader = "module Seath.Test.Exaxmles." <> name <> ".Validator (validatorScript) where"
      validatorType = "validatorScript :: String"
      replacedS = concatMap (\x -> if x == '"' then "\\\"" else [x]) s
      validatorFunction = "validatorScript = \"" <> replacedS <> "\""
   in do
        putStrLn moduleHeader
        putStrLn ""
        putStrLn validatorType
        putStrLn validatorFunction
