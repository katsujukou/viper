module Vite.Export where

import Prelude

import Data.Either (Either, either)
import Data.Maybe (Maybe, maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign (Foreign)
import Foreign.Object (Object)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Record.Unsafe as RU
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)
import Unsafe.Coerce (unsafeCoerce)

type JsValue = Foreign

class Export a where
  export :: a -> JsValue

instance Export Foreign where
  export = identity

instance Export Int where
  export = unsafeCoerce

instance Export Number where
  export = unsafeCoerce

instance Export String where
  export = unsafeCoerce

instance Export Boolean where
  export = unsafeCoerce

instance Export a => Export (Array a) where
  export = unsafeCoerce <<< map export

instance Export a => Export (Object a) where
  export = unsafeCoerce <<< map export 

foreign import jsNull :: JsValue

foreign import jsUndefined :: JsValue

instance Export a => Export (Maybe a) where
  export = maybe jsNull export 

instance (Export a, Export b) => Export (Either a b) where
  export = either export export 

instance 
  ( RowToList r rl
  , ExportRecordProp rl r ro
  ) => Export (Record r)
  where
    export = exportRecordProp (Proxy@rl) >>> unsafeCoerce

class ExportRecordProp (rl :: RowList Type) ri ro  | rl ri -> ro where
  exportRecordProp :: Proxy rl -> Record ri -> Record ro

instance 
  ( TypeEquals r ()
  ) => ExportRecordProp RL.Nil r r
  where
    exportRecordProp _ _ = unsafeCoerce {}

else instance 
  ( IsSymbol label
  , ListToRow tail irest
  , Row.Cons label typ irest ri
  , Row.Lacks label irest
  , Export typ
  , ExportRecordProp tail irest orest
  , Row.Cons label JsValue orest ro
  ) => ExportRecordProp (RL.Cons label typ tail) ri ro
  where
    exportRecordProp _ ri = 
      let label = Proxy@label
          v = Record.get label ri
          irest = Record.delete label ri
          orest = exportRecordProp (Proxy@tail) irest
      in RU.unsafeSet (reflectSymbol label) (export v) orest
