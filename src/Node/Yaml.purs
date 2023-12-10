module Node.Yaml where

import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Effect.Exception (Error)

foreign import parseImpl :: Fn3 (forall a b. a -> Either a b) (forall a b. b -> Either a b) String (Either Error Json)

parse :: String -> Either Error Json 
parse = runFn3 parseImpl Left Right  