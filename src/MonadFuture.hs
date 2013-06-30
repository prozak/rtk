{-# LANGUAGE TypeFamilies, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, RecursiveDo, FunctionalDependencies, GeneralizedNewtypeDeriving,
    StandaloneDeriving  #-}
module MonadFuture(MonadFuture, FutureState, FutureT, future, present, (~=), (%=), runFutureTRec, runFutureT)
    where

import Control.Monad.State.Lazy
import Data.Lens.Common
import Debug.Trace
import Control.Monad.Trans

data FutureState a = FutureState { 
                                 sFuture :: a,
                                 sPresent :: a
                                 }

newtype FutureT s m a = FutureT { fromFutureT :: StateT (FutureState s) m a }
    deriving (Monad, MonadTrans)

class MonadFuture s m | m -> s where
    getFuture :: m s
    getPresent :: m s
    modifyPresent :: (s -> s) -> m ()

instance Monad m => MonadFuture s (FutureT s m) where
    getFuture = FutureT $ gets sFuture
    getPresent = FutureT $ gets sPresent
    modifyPresent func = FutureT $ modify $ \ ~(FutureState f p) -> FutureState f (func p)

deriving instance MonadFix m => MonadFix (FutureT s m)

future :: (Monad m, MonadFuture a m) => Lens a b -> m b
future l = getFuture >>= return . (getL l) 

present :: (Monad m, MonadFuture a m) => Lens a b -> m b
present l = getPresent >>= return . (getL l) 

infixr 4 ~=

(~=):: (Monad m, MonadFuture a m) => Lens a b -> b -> m ()
l ~= b = modifyPresent $ \ sw -> setL l b sw

infixr 4 %=
    
(%=) :: (Monad m, MonadFuture a m) => Lens a b -> (b -> b) -> m ()
l %= f  = modifyPresent $ \ sw -> setL l (f (getL l sw)) sw

runFutureTRec :: (Monad m, MonadFix m) => s -> FutureT s m a -> m (a, s)
runFutureTRec startState comp = do
  rec ~(res, ~(FutureState _ outState)) <- runStateT (fromFutureT comp) (FutureState outState startState)
  return (res, outState)

runFutureT :: (Monad m) => s -> FutureT s m a -> m a
runFutureT startState comp = do
  (res, _) <- runStateT (fromFutureT comp) (FutureState startState startState)
  return res


