-- | Defines a project-specific "prelude".
--
module Base (module X) where

import Control.DeepSeq as X
import Control.Monad as X
import Control.Monad.Except as X
import Control.Monad.Identity as X
import Control.Monad.Reader as X
import Control.Monad.State.Strict as X
import Data.Coerce as X
import Data.Containers.ListUtils as X
import Data.IORef as X
import Data.Foldable as X
import Data.Kind as X
import Data.List as X
import Data.List.Extra as X (groupOn)
import Data.Map.Strict as X (Map, (!))
import Data.Maybe as X
import Data.List.NonEmpty as X (NonEmpty(..), nonEmpty)
import Data.Proxy as X
import Data.Set as X (Set)
import Data.String as X
import Data.Text as X (Text)
import Data.TreeDiff.Class as X (ToExpr)
import Data.Void as X
import GHC.Generics as X (Generic)
import Optics.AffineFold as X
import Optics.Getter as X
import Optics.Lens as X
import Optics.Prism as X
import Optics.Setter as X
import Optics.State as X
import Prettyprinter as X (Doc, Pretty(..), (<+>))
import System.IO as X
import Language.LSP.Protocol.Types as X (NormalizedUri, toNormalizedUri, fromNormalizedUri, Uri(..))
