module Viper.Types where

import Prelude

import Data.Newtype (class Newtype)


newtype PackageName = PackageName String

derive instance Newtype PackageName _
derive instance Eq PackageName
derive instance Ord PackageName

instance Show PackageName where
  show (PackageName p) = "(PackageName " <> p <> ")"