module Test.Vite.Main where

import Prelude

import Effect (Effect)
import Vite (UserConfig)
import Vite.Export (export)
import Vite.Internal (optional)

foreign import spy :: forall a. String -> a -> a 

type T = 
  { foo :: Int
  , bar :: String
  , baz ::
    { quz :: Number
    , qux :: Array Boolean 
    }
  }

main :: Effect Unit
main = do
  _ <- pure $ spy "export3" $ export $
    ((optional @UserConfig
      { root: "root"
      , server: { strictPort: true }
      , build: { outDir: "outDir"}
      }) ) 

  pure unit
