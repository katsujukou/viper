module Vite.Plugin.RunHook 
  ( class IsHookReturnType
  , class RunHook
  , toHookReturnType
  , HookReturnType
  , runHook
  )
  where

import Prelude

import Data.Identity (Identity(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Promise.Aff as Promise

-- \ An opaque type representing some JS value returned from hook functions.
foreign import data HookReturnType :: Type

-- | Converting PureScript typed value into opaque JS value
class IsHookReturnType a where
  toHookReturnType :: a -> HookReturnType

class RunHook m b | m -> b where
  runHook :: forall a. IsHookReturnType a => m a -> Effect b 

instance runHookFunction :: RunHook Identity HookReturnType where
  runHook a = let Identity b = map toHookReturnType a in pure b

instance runHookEffect :: RunHook Effect HookReturnType where
  runHook = map toHookReturnType

instance runHookAff :: RunHook Aff (Promise.Promise HookReturnType) where
  runHook = map toHookReturnType >>> Promise.fromAff

