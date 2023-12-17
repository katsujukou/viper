module Vite.Internal
  ( Vite
  , ViteDevServer
  , class DropIfExists
  , class DropIfExistsRowList
  , class FindWithDefault
  , class FindWithDefaultRowList
  , class Optional
  , class OptionalFields
  , class OptionalFieldsRowList
  , mkVite
  , optional
  , optionalFields
  , optionalFieldsRowList
  )
  where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Promise (Promise)
import Promise.Aff as Promise
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)
import Unsafe.Coerce (unsafeCoerce)
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


class Optional (@r :: Type) a b | r a -> b where
  optional :: a -> b

instance 
  ( OptionalFields r ri ro
  ) => Optional (Record r) (Record ri) (Maybe (Record ro))
  where
    optional = Just <<< optionalFields (Proxy@r)

else instance Optional a a (Maybe a) where
  optional = Just

else instance Optional a b (Maybe b) where
  optional _ = Nothing

class OptionalFields (r :: Row Type) ri ro | r -> ro where
  optionalFields :: Proxy r -> Record ri -> Record ro

instance 
  ( RowToList r rl 
  , OptionalFieldsRowList r rl ri ro
  ) => OptionalFields r ri ro
  where
    optionalFields = optionalFieldsRowList (Proxy@rl)

class OptionalFieldsRowList (r :: Row Type) (rl :: RowList Type) ri ro | r rl -> ro where
  optionalFieldsRowList :: Proxy rl -> Proxy r -> Record ri -> Record ro 

instance 
  ( TypeEquals re ()
  ) => OptionalFieldsRowList r RL.Nil re re where
  optionalFieldsRowList _ _ _ = unsafeCoerce {}

else instance 
  ( ListToRow tail rest
  , Row.Cons label typ rest r
  , DropIfExists label ri irest 
  , OptionalFieldsRowList rest tail irest orest
  , FindWithDefault label ri typ typ'
  , IsSymbol label
  , Optional typ typ' op
  , Row.Cons label op orest ro 
  ) => OptionalFieldsRowList r (RL.Cons label typ tail) ri ro
  where
    optionalFieldsRowList _ _ ri =
      let obj = unsafeCoerce ri
          key = reflectSymbol (Proxy@label)
          (mb :: Maybe typ') = Object.lookup key obj
          orest = optionalFieldsRowList (Proxy@tail) (Proxy@rest) ((unsafeCoerce ri) :: { | irest })
      in
      case mb of
        Nothing -> unsafeCoerce orest # Object.insert key (unsafeCoerce Nothing) # unsafeCoerce
        Just v -> unsafeCoerce orest # Object.insert key (optional@typ v) # unsafeCoerce 
      
class DropIfExists :: Symbol -> Row Type -> Row Type -> Constraint
class DropIfExists label ri ro | label ri -> ro

instance
  ( RowToList ri li
  , DropIfExistsRowList label li lo
  , ListToRow lo ro
  ) => DropIfExists label ri ro

class DropIfExistsRowList :: Symbol -> RowList Type -> RowList Type -> Constraint
class DropIfExistsRowList label li lo | label li -> lo

instance DropIfExistsRowList label RL.Nil RL.Nil
else instance
  ( DropIfExistsRowList label tail lo
  ) => DropIfExistsRowList label (RL.Cons label typ tail) lo
else instance
  ( DropIfExistsRowList label tail lo' 
  ) => DropIfExistsRowList label (RL.Cons label' typ tail) (RL.Cons label' typ lo')

class FindWithDefault :: Symbol -> Row Type -> Type -> Type -> Constraint
class FindWithDefault label r def typ | label r def -> typ

instance
  ( RowToList r rl
  , FindWithDefaultRowList label rl def typ
  ) => FindWithDefault label r def typ

class FindWithDefaultRowList :: Symbol -> RowList Type -> Type -> Type -> Constraint
class FindWithDefaultRowList label rl def typ | label rl def -> typ

instance FindWithDefaultRowList label RL.Nil def def 
else instance FindWithDefaultRowList label (RL.Cons label typ tail) def typ
else instance 
  ( FindWithDefaultRowList label tail def typ
  ) => FindWithDefaultRowList label (RL.Cons label' typ' tail) def typ