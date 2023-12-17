module Vite.Setup where

import Prelude

import Unsafe.Coerce (unsafeCoerce)
import Vite.Export (class Export, export)
import Vite.Internal (class Optional, optional)
import Vite.Options (UserConfig)

foreign import data Config :: Type 
defineConfig
  :: forall conf conf'
   . Optional UserConfig conf conf'
  => Export conf'
  => conf
  -> Config
defineConfig = unsafeCoerce <<< export <<< optional@UserConfig 
