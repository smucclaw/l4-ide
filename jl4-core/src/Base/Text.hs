module Base.Text (module X, Base.Text.show) where

import Data.Text as X
import Data.Text.IO as X

show :: (Show a) => a -> Text
show = pack . Prelude.show
