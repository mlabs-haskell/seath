module Main (main) where

import AdditionValidator (additionScript)
import Serialize (toStringEnvelope)
import Prelude

main :: IO ()
main = do
  writeScriptTo (toStringEnvelope additionScript) "todo"

writeScriptTo :: String -> FilePath -> IO ()
writeScriptTo s _path =
  -- TODO: https://github.com/mlabs-haskell/seath/issues/8
  putStrLn $ "Serialized script:\n" <> s
