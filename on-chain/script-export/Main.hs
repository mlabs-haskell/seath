module Main (main) where

import AdditionValidator (additionScript)
import Data.Eq ((==))
import Data.Foldable (concatMap)
import Data.List (unlines)
import Data.Monoid ((<>))
import Data.String (String)
import Serialize (toStringEnvelope)
import System.IO (IO, putStrLn, writeFile)

main :: IO ()
main = do
  writeScriptTo (toStringEnvelope additionScript) "Addition"

writeScriptTo :: String -> String -> IO ()
writeScriptTo s name =
  -- TODO: https://github.com/mlabs-haskell/seath/issues/8
  let moduleHeader = "module Seath.Test.Examples." <> name <> ".Validator (validatorScript) where"
      validatorType = "validatorScript :: String"
      replacedS = concatMap (\x -> if x == '"' then "\\\"" else [x]) s
      validatorFunction = "validatorScript = \"" <> replacedS <> "\""
      pursFilePath = "off-chain/test/Examples/Addition/Validator.purs"
      fileContent =
        unlines
          [ moduleHeader
          , ""
          , validatorType
          , validatorFunction
          ]
   in do
        writeFile pursFilePath fileContent
        putStrLn moduleHeader
        putStrLn ""
        putStrLn validatorType
        putStrLn validatorFunction
