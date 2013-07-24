module RefTable(RefTable, Ref, empty, emptyWithError, errorRef, newRef, RefTable.lookup, update, values)
    where

import Data.Lens.Common
import Data.Lens.Template
import MonadFuture
import Data.Maybe
import Debug.Trace
import DocQuote

import qualified Data.IntMap.Lazy as IM
import qualified Data.Map.Lazy as M

data RefTable a = RefTable Int (IM.IntMap a)

newtype Ref a = Ref { fromRef :: Int }
    deriving Show

empty :: RefTable a
empty = RefTable 1 (IM.empty)

emptyWithError :: a -> RefTable a
emptyWithError err = RefTable 1 (IM.insert 0 err $ IM.empty)

errorRef :: Ref a
errorRef = Ref 0

newRef_ :: RefTable a -> a -> (RefTable a, Ref a)
newRef_ (RefTable i tab) val = (RefTable (i + 1) $ IM.insert i val tab, Ref i)

newRef :: (Monad m, MonadFuture s m) => Lens s (RefTable a) -> a -> m (Ref a)
newRef tabLens val = do
  oldTab <- present tabLens
  let ~(newTab, ref) = newRef_ oldTab val
  tabLens ~= newTab
  return ref

lookup_ :: Ref a -> RefTable a -> Maybe a
lookup_ (Ref i) (RefTable _ m) = IM.lookup i m

lookup :: (Monad m, MonadFuture s m) => (Lens s (RefTable a) -> m (RefTable a)) -> Ref a -> Lens s (RefTable a) -> m (Maybe a)
lookup when ref tab =
  when tab >>= return . lookup_ ref

update_ :: Ref a -> (a -> a) -> RefTable a -> RefTable a
update_ (Ref key) func (RefTable i tab) = RefTable i (IM.update (Just . func) key tab)

update :: (Monad m, MonadFuture s m) => Ref a -> (a -> a) -> Lens s (RefTable a) -> m ()
update ref func tab =
  tab %= update_ ref func

values_ :: RefTable a -> [a]
values_ (RefTable _ tab) = map snd $ tail $ IM.toAscList tab

values :: (Monad m, MonadFuture s m) => (Lens s (RefTable a) -> m (RefTable a)) -> Lens s (RefTable a) -> m [a]
values when tab = 
    when tab >>= return . values_

