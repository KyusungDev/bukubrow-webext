module AppM where

import Prelude

import Capability.Logger (class Logger, LogType(..))
import Capability.Settings (class Settings)
import Control.Monad.Reader (class MonadAsk, ReaderT, asks, runReaderT)
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Env (Env)
import Settings (getAll, setAll)
import Type.Equality as TE

newtype AppM a = AppM (ReaderT Env Aff a)

derive instance newtypeAppM :: Newtype (AppM a) _
derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TE.TypeEquals e Env => MonadAsk e AppM where
    ask = AppM $ asks TE.from

runAppM :: Env -> AppM ~> Aff
runAppM e (AppM m) = runReaderT m e

instance loggerAppM :: Logger AppM where
    log logType msg = fmt logType (unwrap msg) # Console.log # liftEffect
        where
            fmt :: LogType -> String -> String
            fmt x y = prefix x <> " " <> y

            prefix :: LogType -> String
            prefix Debug = "[DEBUG]"
            prefix Info  = "[INFO]"
            prefix Warn  = "[WARN]"
            prefix Error = "[ERROR]"


instance settingsAppM :: Settings AppM where
    getSettings = getAll >>> liftAff
    setSettings = setAll >>> liftAff

