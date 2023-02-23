{-# LANGUAGE TemplateHaskell #-}

module TestScript (testScript) where

import Plutus.V2.Ledger.Api (Script, fromCompiledCode)
import PlutusTx (BuiltinData, compile)

alwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> ()
alwaysSucceeds _ _ _ = ()

testScript :: Script
testScript = fromCompiledCode $$(compile [||alwaysSucceeds||])
