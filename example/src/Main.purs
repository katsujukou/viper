module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import UUID (genUUID)

main :: Effect Unit
main = do
  Console.log "🍝"
  Console.logShow =<< genUUID
