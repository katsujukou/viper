module Vite.Plugin.Config where

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object (Object)
import Node.Path (FilePath)
import Type.Row (type (+))
import Vite.Export (class Export, JsValue, export, jsNull)
import Vite.Plugin.Internal (Hook, VitePlugin, registerHook1, registerHook3, registerProperty)

data EnforceOrder = Pre | Post

instance Export EnforceOrder where
  export = export <<< case _ of
    Pre -> "pre"
    Post -> "post"

enforce
  :: forall hooks
   . EnforceOrder
  -> VitePlugin hooks
  -> VitePlugin hooks
enforce = registerProperty @"enforce"

-- | resolveId hook
type RESOLVE_ID = "resolveId"

type ResolveId (m :: Type -> Type) r = (resolveId :: m | r)

type ResolveIdArgs = 
  { source :: String
  , importer :: Maybe FilePath
  , options :: 
    -- Todo 
    { attributes :: Object String
    , ssr :: Boolean
    }
  }

data ResolveIdResult 
  = ResolveIdString String
  | ResolveIdObject { id :: String }
  | ResolveIdNull 
  
instance Export ResolveIdResult where
  export = case _ of
    ResolveIdString s -> export s
    ResolveIdObject o -> export o
    _ -> jsNull

onResolveId
  :: forall hin hookM
   . MonadAff hookM
  => Hook ResolveIdArgs ResolveIdResult hookM
  -> VitePlugin hin 
  -> VitePlugin (ResolveId hookM + hin)
onResolveId f = registerHook3 @RESOLVE_ID @"source" @"importer" @"options" f

-- | Load hook 
type LOAD = "load"

type Load (m :: Type -> Type) r = (load :: m | r)

type LoadArgs = 
  { id :: String
  }

type SourceDescriptor =
  { code :: String
  , map :: Sourcemap
  }

-- TODO better type implementation
type Sourcemap = JsValue

data LoadResult 
  = LoadString String
  | LoadSourceDesc SourceDescriptor
  | LoadNull

instance Export LoadResult where
  export = case _ of
    LoadString s -> export s
    LoadSourceDesc d -> export d
    _ -> jsNull

onLoad
  :: forall hin hookM
   . Hook LoadArgs LoadResult hookM
  -> VitePlugin hin 
  -> VitePlugin (Load hookM + hin)
onLoad f = registerHook1 @LOAD @"id" f