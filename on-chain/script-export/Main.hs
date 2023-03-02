module Main (main) where

import AdditionValidator (additionScript, datum1, datum2, redeemer1)
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
      pursFilePath = "off-chain/test/Examples/Addition/Validator.purs"
      fileContent =
        mconcat $
          map
            (++ "\n")
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
        putStrLn datum1
        putStrLn datum2
        putStrLn redeemer1
