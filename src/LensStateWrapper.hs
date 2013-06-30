{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts  #-}
module LensStateWrapper
    where

import Control.Monad.State.Lazy
import Data.Lens.Common
import Debug.Trace

access :: (Monad m, MonadState (a, a) m) => Lens a b -> m b
access l | trace "access " True = gets $ (getL l) . fst

access' :: (Monad m, MonadState (a, a) m) => Lens a b -> m b
access' l  | trace "access' " True = gets $ (getL l) . snd

infixr 4 ~=

(~=):: (Monad m, MonadState (a, a) m) => Lens a b -> b -> m ()
l ~= b = modify $ \ ~(sr, sw) -> (sr, setL l b sw)

infixr 4 %=
    
(%=) :: (Monad m, MonadState (a, a) m) => Lens a b -> (b -> b) -> m ()
l %= f  | trace "%= " True = modify $ \ ~(sr, sw) -> (sr, setL l (f (getL l sw)) sw)