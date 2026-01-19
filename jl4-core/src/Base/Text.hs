module Base.Text (module X, textShow) where

import Data.Text as X
import Data.Text.IO as X

-- | Convert a value to Text using its Show instance
-- Named textShow to avoid conflict with Prelude.show (added in text >= 2.1.2)
textShow :: (Show a) => a -> Text
textShow = pack . Prelude.show
