{-# LANGUAGE CPP #-}
module Base.Text (module X, textShow) where

#if MIN_VERSION_text(2,1,2)
import Data.Text as X hiding (show)
#else
import Data.Text as X
#endif
import Data.Text.IO as X

-- | Convert a value to Text using its Show instance
-- Named textShow to avoid conflict with Prelude.show (added in text >= 2.1.2)
textShow :: (Show a) => a -> Text
textShow = pack . Prelude.show
