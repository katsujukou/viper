module Vite.TypeUtils where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)
import Unsafe.Coerce (unsafeCoerce)

class Optional (@r :: Type) a b | r a -> b where
  optional :: a -> b

instance 
  ( OptionalFields r ri ro
  ) => Optional (Record r) (Record ri) (Maybe (Record ro))
  where
    optional = Just <<< optionalFields (Proxy@r)

else instance Optional a a (Maybe a) where
  optional = Just

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
  ( TypeEquals ro ()
  ) => OptionalFieldsRowList re RL.Nil ri ro where
  optionalFieldsRowList _ _ _ = unsafeCoerce {}

else instance 
  ( ListToRow tail rest
  , Row.Cons label typ rest r
  , OptionalFieldsRowList rest tail ri orest
  , IsSymbol label
  , Optional typ typ op
  , Row.Cons label op orest ro 
  ) => OptionalFieldsRowList r (RL.Cons label typ tail) ri ro
  where
    optionalFieldsRowList _ _ ri =
      let obj = unsafeCoerce ri
          key = reflectSymbol (Proxy@label)
          (mb :: Maybe typ) = Object.lookup key obj
          orest = optionalFieldsRowList (Proxy@tail) (Proxy@rest) ri
      in
      case mb of
        Nothing -> unsafeCoerce orest # Object.insert key (unsafeCoerce Nothing) # unsafeCoerce
        Just v -> unsafeCoerce orest # Object.insert key (optional@typ v) # unsafeCoerce 
      