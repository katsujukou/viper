module Viper.Monad
  ( ViperM
  , runViperM
  , module Viper.Env
  , module Viper.Error
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, runReaderT)
import Data.Either (Either)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Viper.Env (Env)
import Viper.Error (ViperError)

newtype ViperM a = ViperM (ReaderT Env (ExceptT ViperError Aff) a)

derive newtype instance Functor ViperM
derive newtype instance Apply ViperM 
derive newtype instance Applicative ViperM 
derive newtype instance Bind ViperM 
derive newtype instance Monad ViperM 
derive newtype instance MonadEffect ViperM 
derive newtype instance MonadAff ViperM
derive newtype instance MonadAsk Env ViperM 
derive newtype instance MonadReader Env ViperM 
derive newtype instance MonadThrow ViperError ViperM 
derive newtype instance MonadError ViperError ViperM

runViperM :: forall a. Env -> ViperM a -> Aff (Either ViperError a)
runViperM env (ViperM m) = runExceptT $ runReaderT m env