module Vite.Internal 
  ( ViteDevServer
  , Vite
  , mkVite 
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Aff (Aff)
import Promise (Promise)
import Promise.Aff as Promise
import Vite.Export (export, JsValue)
import Vite.Options (UserConfig)

type ViteDevServer = 
  { close :: Aff Unit
  }

type Vite =
  { serve :: UserConfig -> Aff ViteDevServer
  }

mkVite :: UserConfig -> Aff Vite
mkVite config = runFn2 mkViteImpl Promise.toAffE (export config)

foreign import mkViteImpl
  :: Fn2
    (forall a. Effect (Promise a) -> Aff a)
    JsValue
    (Aff Vite)

