module Vite.Options.Server where

import Data.Either (Either)

type Options = 
  { host :: Either Boolean String
  , port :: Int
  , strictPort :: Boolean 
  }
