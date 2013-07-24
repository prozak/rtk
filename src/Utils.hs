module Utils
    where

import Data.Lens.Common
import Data.Lens.Template
import MonadFuture
import Data.Maybe
import Debug.Trace
import DocQuote
import Text.PrettyPrint
import RefTable

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

checkMaybeRef :: (MonadGenError m) => Maybe a -> a -> String -> m a
checkMaybeRef mVal errVal errStr = do
  let res = maybe errVal id mVal
  logError $ maybe (Just errStr) (const Nothing) mVal
  return res

silentCheckMaybeRef :: Monad m => Maybe a -> a -> m a
silentCheckMaybeRef mVal errVal = do
  let res = maybe errVal id mVal
  return res
