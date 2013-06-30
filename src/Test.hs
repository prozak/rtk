{-# LANGUAGE TypeFamilies, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, RecursiveDo  #-}
module Test
    where

import Control.Monad.State.Lazy
import Data.Lens.Common
import Data.Lens.Template
import LensStateWrapper
import Data.Maybe
import Debug.Trace
import Control.Monad.Identity

import qualified Data.Map as M

data TestState = TestState {
                            _testVal :: M.Map Int Int
                           }

$(makeLens ''TestState)

newtype TestM m a = TestM { fromTestM :: StateT (TestState, TestState) m a }

instance Monad m => Monad (TestM m) where
    return = TestM . return
    a >>= b = TestM (fromTestM a >>= fromTestM . b) 

instance Monad m => MonadState (TestState, TestState) (TestM m) where
    get = TestM get
    put = TestM . put
    state = TestM . state

instance MonadFix m => MonadFix (TestM m) where
    mfix f = TestM $ mfix (fromTestM . f)

runTestM :: (Monad m, MonadFix m) => TestM m a -> m a
runTestM t = do
  rec (res, (_, outV)) <- runStateT (fromTestM t) (outV, TestState M.empty)
  return res

test :: Monad m => TestM m Int
test = do
  testVal %= (M.insert 1 1)
  ~(Just val) <- access testVal >>= return . M.lookup 2
  testVal %= (M.insert 2 2)
  return val