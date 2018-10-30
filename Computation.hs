{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, RankNTypes, TypeApplications, TypeOperators #-}
module Computation where

import           Base
import           Control.Monad
import qualified Control.Effect as HEffect
import qualified Control.Effect.State as HEffect
import qualified Control.Monad.Effect as Effect
import qualified Control.Monad.Effect.State as Effect
import           Control.Monad.Free.VanLaarhovenE
import           Control.Monad.State.Class
import qualified Control.Monad.State.Strict as MTL
import qualified Fused
import GHC.Generics

{- It is only fair to give the computations that use a free monad the same
advantage as MTL, namely that they become specialized to the concrete monad
in question. Indeed, the results for msComputation+MTL.State below (for
which there is no SPECIALIZE pragma) demonstrate that this makes MTL a lot
slower.

This does not help much for most of the free monad implementations. But it
gives huge wins for Fused (and MTL).

Specialization can be done manually, as with mtlComputation and fusedComputation
below. Or you can let the compiler do it with a SPECIALIZE pragma, see e.g. the
one for msComputation+Fused below.
-}

heffComputation :: (HEffect.Carrier sig m, HEffect.Effect sig) => Int -> HEffect.Eff (HEffect.StateC Int m) ()
heffComputation n = forM_ [1..n]  $ \_ -> do
  s <- HEffect.get @Int
  HEffect.put $! (s + 1)

effComputation :: Int -> Effect.Eff '[Effect.State Int] ()
effComputation n = forM_ [1..n]  $ \_ -> do
  s <- Effect.get @Int
  Effect.put $! (s + 1)

computation :: MonadFree F m => Int -> m ()
computation n = forM_ [1..n] $ \_ -> do
  s <- Base.get
  Base.put $! s + 1

msComputation :: MonadState Int m => Int -> m ()
msComputation n = forM_ [1..n] $ \_ -> do
  s <- MTL.get
  MTL.put $! s + 1
{-# SPECIALIZE msComputation :: Int -> Fused.Codensity (Fused.StateCarrier Int (Fused.Free Fused.Void)) () #-}

mtlComputation :: Int -> MTL.State Int ()
mtlComputation n = forM_ [1..n] $ \_ -> do
  s <- MTL.get
  MTL.put $! s + 1

fusedComputation :: Int -> Fused.Codensity (Fused.StateCarrier Int (Fused.Free Fused.Void)) ()
fusedComputation n = forM_ [1..n] $ \_ -> do
  s <- MTL.get
  MTL.put $! s + 1

computation2 :: MonadFree F m => Int -> m ()
computation2 n =
  if n == 0
    then return ()
    else do
      computation2 (n-1)
      s <- Base.get
      Base.put $! s + 1

heffComputation2 :: (HEffect.Carrier sig m, HEffect.Effect sig) => Int -> HEffect.Eff (HEffect.StateC Int m) ()
heffComputation2 n =
  if n == 0
    then return ()
    else do
      heffComputation2 (n - 1)
      s <- HEffect.get @Int
      HEffect.put $! s + 1

effComputation2 :: Int -> Effect.Eff '[Effect.State Int] ()
effComputation2 n =
  if n == 0
    then return ()
    else do
      effComputation2 (n - 1)
      s <- Effect.get @Int
      Effect.put $! s + 1

msComputation2 :: MonadState Int m => Int -> m ()
msComputation2 n =
  if n == 0
    then return ()
    else do
      msComputation2 (n-1)
      s <- MTL.get
      MTL.put $! s + 1
{-# SPECIALIZE msComputation2 :: Int -> Fused.Codensity (Fused.StateCarrier Int (Fused.Free Fused.Void)) () #-}


mtlComputation2 :: Int -> MTL.State Int ()
mtlComputation2 n =
  if n == 0
    then return ()
    else do
      mtlComputation2 (n-1)
      s <- MTL.get
      MTL.put $! s + 1

fusedComputation2 :: Int -> Fused.Codensity (Fused.StateCarrier Int (Fused.Free Fused.Void)) ()
fusedComputation2 n =
  if n == 0
    then return ()
    else do
      msComputation2 (n-1)
      s <- MTL.get
      MTL.put $! s + 1

data State s m = State { getState :: m s, putState :: s -> m () }

get_ :: HasEffect effects (State s) => Free effects s
get_  = liftF getState

put_ :: HasEffect effects (State s) => s -> Free effects ()
put_ s = liftF (\st -> putState st s)

vlComputation :: (HasEffect effects (State Int)) => Int -> Free effects ()

vlComputation n = forM_ [1..n] $ \_ -> do
    s <- get_
    put_ $! s + (1::Int)

myState :: State s (MTL.State s)
myState = State {getState = MTL.get, putState = MTL.put}

stateInterp = myState .:. EmptyE

vl  :: Free '[State Int] a -> MTL.State Int a
vl = iterM stateInterp
