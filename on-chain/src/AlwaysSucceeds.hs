{-# LANGUAGE TemplateHaskell#-}

module AlwaysSucceeds (alwaysSucceedsCompiled, plc, serializedCompiled) where


import PlutusLedgerApi.Common 
import PlutusTx

alwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> ()
alwaysSucceeds _ _ _ = ()

alwaysSucceedsCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
alwaysSucceedsCompiled = $$(compile [|| alwaysSucceeds ||])

plc :: String
plc = show $ getPlc alwaysSucceedsCompiled

serializedCompiled :: SerialisedScript
serializedCompiled = serialiseCompiledCode alwaysSucceedsCompiled
