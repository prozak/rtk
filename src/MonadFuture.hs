{-# LANGUAGE TypeFamilies, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, RecursiveDo, FunctionalDependencies, GeneralizedNewtypeDeriving,
    StandaloneDeriving  #-}
module MonadFuture(MonadFuture, FutureState, FutureT, future, present, (~=), (%=), runFutureTRec, runFutureT, ifE, MonadCond)
    where

import Control.Monad.State.Lazy
import Data.Lens.Common
import Debug.Trace
import Control.Monad.Trans

data FutureState a = FutureState { 
                                 sFuture :: a,
                                 sPresent :: a
                                 }

data FutureT s m a = FutureT { runFutureTr :: (FutureState s) -> m (a, FutureState s) }

instance Monad m => Monad (FutureT s m) where
    -- (>>=) :: forall a b. m a -> (a -> m b) -> m b
    (FutureT f1) >>= func = FutureT (\s -> do
                                       ~(a, s) <- f1 s
                                       runFutureTr (func a) s)

    -- return :: a -> m a
    return a = FutureT (\s -> return (a, s))

instance MonadTrans (FutureT s) where
    -- lift :: Monad m => m a -> t m a
    lift comp = FutureT (\s -> do
                           res <- comp
                           return (res, s))

class Monad m => MonadFuture s m | m -> s where
    getFuture :: m s
    getPresent :: m s
    modifyPresent :: (s -> s) -> m ()

class Monad m => MonadCond m where
    ifE :: Bool -> m a -> m a -> m a

instance Monad m => MonadFuture s (FutureT s m) where
    getFuture = FutureT $ \s -> return (sFuture s, s)
    getPresent = FutureT $ \s -> return (sPresent s, s)
    modifyPresent func = FutureT $ \ ~(FutureState f p) -> return ((), FutureState f (func p))

instance Monad m => MonadCond (FutureT s m) where
    ifE cond ~(FutureT tF) ~(FutureT fF) = FutureT $ \ s -> do
                                              ~(trueA, FutureState trueFuture truePresent) <- tF s 
                                              ~(falseA, FutureState falseFuture falsePresent) <- fF s 
                                              return (if cond then trueA else falseA,
                                                      FutureState (if cond then trueFuture else falseFuture) 
                                                                  (if cond then truePresent else falsePresent))

instance MonadFix m => MonadFix (FutureT s m) where
    mfix func = FutureT (\s -> mfix $ \ ~(a, _) -> runFutureTr (func a) s)

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
  rec ~(res, ~(FutureState _ outState)) <- runFutureTr comp (FutureState outState startState)
  return (res, outState)

runFutureT :: (Monad m) => s -> FutureT s m a -> m a
runFutureT startState comp = do
  (res, _) <- runFutureTr comp (FutureState startState startState)
  return res


