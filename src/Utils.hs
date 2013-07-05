module Utils
    where

import Data.Lens.Common
import Data.Lens.Template
import MonadFuture
import Data.Maybe
import Debug.Trace
import DocQuote
import Text.PrettyPrint

import qualified Data.IntMap.Lazy as IM
import qualified Data.Map.Lazy as M

class Monad m => NameGen m where
    newName :: String -> m String

ensureName :: NameGen m => Maybe String -> String -> m String
ensureName (Just n) str = return n
ensureName Nothing str = newName str

newNameWithCounter :: (Monad m, MonadFuture s m) => (Lens s Int) -> String -> m String
newNameWithCounter counter prefix = do
  ind <- present counter
  counter %= (1 +)
  return $ prefix ++ show ind
