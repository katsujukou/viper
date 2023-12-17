module Vite.Options where

import Vite.Options.Build as Build
import Vite.Options.Common as Common
import Vite.Options.Server as Server

type UserConfig = 
  { server :: Server.Options
  , build :: Build.Options 
  | Common.Options
  }
