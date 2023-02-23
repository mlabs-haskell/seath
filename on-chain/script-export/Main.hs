module Main (main) where

import Serialize
import TestScript
import Prelude

main :: IO ()
main = do
  writeScriptTo (toStringEnvelope testScript) "todo"

writeScriptTo :: String -> FilePath -> IO ()
writeScriptTo s _path =
  -- TODO: https://github.com/mlabs-haskell/seath/issues/8
  putStrLn $ "Serialized script:\n" <> s
