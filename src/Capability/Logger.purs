module Capability.Logger where

import Prelude

import Data.Newtype (class Newtype)
import Halogen (HalogenM, lift)

data LogType
  = Debug
  | Info
  | Warn
  | Error

derive instance eqLogType :: Eq LogType
derive instance ordLogType :: Ord LogType

newtype LogMessage = LogMessage String

derive instance newtypeLogMessage :: Newtype LogMessage _

class Monad m <= Logger m where
    log :: LogType -> LogMessage -> m Unit

instance loggerHalogenM :: Logger m => Logger (HalogenM a b c d m) where
    log x y = lift $ log x y

