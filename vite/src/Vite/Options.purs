module Vite.Options where

import Data.Maybe (Maybe(..))
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Vite.Options.Build as Build
import Vite.Options.Common as Common
import Vite.Options.Server as Server

type UserConfig = 
  { server :: Server.Options
  , build :: Build.Options 
  | Common.Options
  }
