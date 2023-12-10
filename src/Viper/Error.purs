module Viper.Error where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Node.Path (FilePath)

data ViperError
  = NoPackageDesc FilePath
  | UnexpectedSpagoYamlFormat
  | NotImplemented

derive instance Eq ViperError
derive instance Ord ViperError
derive instance Generic ViperError _ 

instance Show ViperError where
  show = genericShow
