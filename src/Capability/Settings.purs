module Capability.Settings where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Settings (Settings)

class Monad m <= Settings m where
    getSettings :: Unit -> m (Maybe Settings)
    setSettings :: Settings -> m Unit

instance settingsHalogenM :: Settings m => Settings (HalogenM a b c d m) where
    getSettings = getSettings >>> lift
    setSettings = setSettings >>> lift

