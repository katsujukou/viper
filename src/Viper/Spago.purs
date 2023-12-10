module Viper.Spago where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut.Core (Json, caseJsonArray, caseJsonObject, caseJsonString)
import Data.Array (findMap, (!!))
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Codec.Argonaut (JsonDecodeError(..))
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Yaml as Yaml
import Viper.Error (ViperError(..))
import Viper.Types (PackageName(..))

data PackageDescType
  = SpagoYaml
  | SpagoDhall
  | BowerJson

derive instance Eq PackageDescType
derive instance Generic PackageDescType _ 
instance Show PackageDescType where
  show = genericShow

filepath :: PackageDesc -> FilePath 
filepath { typ, directory } = Path.concat [directory, descFilename typ]
  where
    descFilename = case _ of
      SpagoYaml -> "spago.yaml"
      SpagoDhall -> "spago.dhall"
      BowerJson -> "bower.json"

type PackageDesc =
  { typ :: PackageDescType
  , directory :: FilePath
  }

packageDescMap :: Map String PackageDescType
packageDescMap = Map.fromFoldable
  [ "spago.yaml" /\ SpagoYaml
  , "spago.dhall" /\ SpagoDhall
  , "bower.json" /\ BowerJson
  ]

findPackageDescriptor 
  :: forall m
   . MonadAff m 
  => MonadThrow ViperError m 
  => FilePath 
  -> m PackageDesc
findPackageDescriptor base = do
  files <- liftAff $ readdir base
  findMap (_ `Map.lookup` packageDescMap) files 
    # maybe 
      (throwError (NoPackageDesc base))
      ({ typ: _, directory: base } >>> pure)

getPackageDependencies
  :: forall m
   . MonadAff m 
  => MonadThrow ViperError m 
  => PackageDesc 
  -> m (Array PackageName)
getPackageDependencies pkgDesc = do
  let pkgDescPath = filepath pkgDesc 
  pkgDescContent <- liftAff $ readTextFile UTF8 pkgDescPath
  case pkgDesc.typ of
    BowerJson -> throwError NotImplemented
    SpagoDhall -> throwError NotImplemented
    SpagoYaml -> do
      Yaml.parse pkgDescContent
        >>= readSpagoYamlJson
  where
    readSpagoYamlJson = 
      jsonProp "package"
        >=> jsonIndex 0
        >=> jsonProp "dependencies" 
        >=> decodeJsonArray (decodePackageName)
    
    decodeJsonArray :: forall a. (Json -> a) -> Json -> Array a
    decodeJsonArray decoder json = caseJsonArray (Left (TypeMismatch "array")) go do
      res <- STArray.new

      
    jsonProp :: String -> Json -> Maybe Json 
    jsonProp prop = caseJsonObject Nothing (Object.lookup prop)

    jsonIndex :: Int -> Json -> Maybe Json 
    jsonIndex n = caseJsonArray Nothing (_ !! n)