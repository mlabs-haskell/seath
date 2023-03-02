module ScriptLoading (loadScript, Path(Path)) where

import Contract.Prelude (class Newtype)
import Contract.Scripts (Validator)
import Undefined (undefined)

newtype Path = Path String

derive instance Newtype Path _

-- | load a script saved in a file 
loadScript :: Path -> Validator
loadScript = undefined

