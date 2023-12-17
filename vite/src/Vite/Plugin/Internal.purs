module Vite.Plugin.Internal
  ( VitePlugin
  , Hook
  , definePlugin
  , registerProperty
  , registerHook1
  , registerHook2
  , registerHook3
  )
  where

import Prelude

import Data.Function.Uncurried (mkFn1, mkFn2, mkFn3)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Record.Builder as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Vite.Export (class Export, export)

foreign import data VitePlugin :: Row (Type -> Type) -> Type

-- | A type for Vite's plugin hook functions.
type Hook (arg :: Type) (ret :: Type) m = arg -> m ret

definePlugin :: String -> VitePlugin ()
definePlugin = Object.singleton "name" >>> unsafeCoerce

registerProperty
  :: forall @prop hooks a
   . IsSymbol prop
  => Export a
  => a
  -> VitePlugin hooks
  -> VitePlugin hooks
registerProperty a = unsafeAsObject (Object.insert (reflectSymbol@prop Proxy) (export a))

registerHook1
  :: forall @hook @label hin hout hookM args ret a 
   . IsSymbol hook
  => IsSymbol label
  => Row.Cons label a () args
  => Row.Cons hook hookM hin hout
  => Hook { | args } ret hookM
  -> VitePlugin hin
  -> VitePlugin hout
registerHook1 f = unsafeAsObject (Object.insert (reflectSymbol@hook Proxy) fn)
  where
    fn = mkFn1 \a -> 
      let
        args = Record.build (Record.insert (Proxy@label) a) {}
      in f args

registerHook2
  :: forall @hook @label1 @label2 hin hout hookM args ret a b _1 
   . IsSymbol hook
  => IsSymbol label1
  => IsSymbol label2
  => Row.Cons label1 a () _1
  => Row.Lacks label2 _1
  => Row.Cons label2 b _1 args
  => Row.Cons hook hookM hin hout
  => Hook { | args } ret hookM
  -> VitePlugin hin
  -> VitePlugin hout
registerHook2 f = unsafeAsObject (Object.insert (reflectSymbol@hook Proxy) fn)
  where
    fn = mkFn2 \a b -> 
      let
        args = Record.build
          ( Record.insert (Proxy@label1) a
          >>> Record.insert (Proxy@label2) b) {}
      in f args

registerHook3 
  :: forall @hook @label1 @label2 @label3 hin hout hookM args ret a b c _1 _2 
   . IsSymbol hook
  => IsSymbol label1
  => IsSymbol label2
  => IsSymbol label3
  => Row.Cons label1 a () _1
  => Row.Lacks label2 _1
  => Row.Cons label2 b _1 _2 
  => Row.Lacks label3 _2
  => Row.Cons label3 c _2 args
  => Row.Cons hook hookM hin hout
  => Hook { | args } ret hookM
  -> VitePlugin hin
  -> VitePlugin hout
registerHook3 f = unsafeAsObject (Object.insert (reflectSymbol@hook Proxy) fn) 
  where
    fn = mkFn3 \a b c -> 
      let
        args = Record.build
          ( Record.insert (Proxy@label1) a
            >>> Record.insert (Proxy@label2) b
            >>> Record.insert (Proxy@label3) c
          ) {}
      in f args

unsafeAsObject :: forall a b r. (Object r -> Object r) -> a -> b
unsafeAsObject f = unsafeCoerce <<< f <<< unsafeCoerce 