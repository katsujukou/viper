module Vite.Options.Build where

import Node.Path (FilePath)
import Vite.Export (class Export, export)

data BuildMinify = BMDisable | BMEsbuild | BMTeaser

instance Export BuildMinify where
  export = case _ of
    BMDisable -> export false
    BMEsbuild -> export "exbuild"
    BMTeaser -> export "teaser"

type Options = 
  { outDir :: FilePath
  , minify :: BuildMinify
  }