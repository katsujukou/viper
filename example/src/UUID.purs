module UUID where

import Prelude

import Effect (Effect)

newtype UUID = UUID String

instance Show UUID where
  show (UUID uuid) = "(UUID " <> uuid <> ")"

foreign import genUUID :: Effect UUID