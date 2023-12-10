module Viper.PackageGraph where

import Prelude

import Data.List (List)
import Node.Path (FilePath)
import Viper.Types (PackageName(..))

type DependencyGraph = Array DependencyGraphNode

type DependencyGraphNode = 
  { name :: PackageName
  , dependsOn :: List PackageName
  }

-- Topological sort
topsort :: DependencyGraph -> DependencyGraph
topsort = identity