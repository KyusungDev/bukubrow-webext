module Entry.Content where

import Prelude

import AppM (runAppM)
import Data.Lens (review)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Env (Env, initial)
import Halogen (hoist)
import Halogen.Aff (awaitBody)
import Halogen.VDom.Driver (runUI)
import Settings (getTheme, themePrism)
import Ui.Components.Onboarding (onboarding)

-- | Get an initial env with anything attached that we want to fetch prior to
-- | the page rendering.
getEnv :: Aff Env
getEnv = do
    theme <- getTheme unit
    pure case theme of
        (Just x) -> initial { prefs { theme = x } }
        Nothing  -> initial

main :: Effect Unit
main = launchAff_ do
    env <- getEnv
    liftEffect $ Console.log $ review themePrism env.prefs.theme
    let root = hoist (runAppM env) onboarding
    awaitBody >>= runUI root unit

