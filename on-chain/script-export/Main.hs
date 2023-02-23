module Main (main) where

import Serialize
import TestScript
import Prelude

main :: IO ()
main = do
  writeScriptTo (toStringEnvelope testScript) "todo"

writeScriptTo :: String -> FilePath -> IO ()
writeScriptTo s _path =
  putStrLn $ "TODO: writing script\n" <> s
