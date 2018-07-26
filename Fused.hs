{- Idea and implementation are from the paper "Fusion for Free: Efficient
Algebraic Effect Handlers" (MPC 2015) by Nicolas Wu and Tom Schrijvers.

Their class 'TermMonad' is already present here under the name
'Base.MonadFree' (modulo swapped type parameters and class method 'con'
renamed 'wrap'). For simplicity, we skip the 'TermAlgebra' class in the
paper and implement the instance for 'MonadFree' directly; this also avoids
the need for UndecidableInstances and other type class unpleasantness.

Since this is only a demo of an efficient free monad implementation and not
an extensible effects library, we don't bother to parameterize the carrier
functor with a base monad, thus further simplifying the code.

The fused representation is the Codensity transform of the carrier functor.

While the authors develop their code starting with the standard free modad
type (exactly as in module Free), it disappears completely after fusion and
is thus not present here.

The correctness of the fusion crucially relies on the stateful computation
(what is passed to 'run') only using the polymorphic API exposed by the
'MonadFree' class.

However, to get decent speed, it is equally crucial that the computation is
specialized to the concrete 'Codensity H' monad. This can be done either
manually or with a compiler pragma. See module Computation for details.
-}

{-# LANGUAGE EmptyCase, RankNTypes, GADTs, UndecidableInstances, FlexibleContexts, TypeOperators #-} -- to enable 'forall' keyword
module Fused (run, Codensity, H, State, StateCarrier(..), TermAlgebra(..), run', Void, Free) where

import Base

import Control.Applicative
import Control.Monad
import Control.Monad.State.Class
import GHC.Generics

-- The state carrier functor; it coincides with F, but using F here instead of
-- H is a lot slower (at least factor 5). I have not yet found out why.
newtype H a = H { unH :: Int -> (Int, a) }

data State s a where
  Get :: (s -> a) -> State s a
  Put :: s -> a -> State s a

getState :: TermAlgebra f (State s :+: g) => Codensity f s
getState = con (L1 (Get pure))

putState :: TermAlgebra f (State s :+: g) => s -> Codensity f ()
putState s = con (L1 (Put s (pure ())))

class Functor f => TermAlgebra h f | h -> f where
  var :: a -> h a
  con :: f (h a) -> h a

genState :: (Monad m, TermAlgebra m f) => a -> (s -> m a)
genState x = const (var x)

algState :: (Monad m, TermAlgebra m f) => State s (s -> m a) -> (s -> m a)
algState (Put s k) _ = k s
algState (Get k) s = k s s

instance Functor (State s) where
  fmap f (Get k) = (Get (f . k))
  fmap f (Put s a) = (Put s (f a))

newtype StateCarrier s m a = SC { unSC :: s -> m a }

data Free f a = Var a | Con (f (Free f a))

instance Functor f => Monad (Free f) where
  return = pure
  {-# INLINE return #-}
  Var a >>= f = f a
  Con m >>= f = Con ((>>= f) <$> m)

instance Functor f => Applicative (Free f) where
  pure = Var
  {-# INLINE pure #-}
  Var a <*> Var b = Var $ a b
  Var a <*> Con mb = Con $ fmap a <$> mb
  Con ma <*> b = Con $ (<*> b) <$> ma

instance Functor f => Functor (Free f) where
  fmap f = go where
    go (Var a)  = Var (f a)
    go (Con fa) = Con (go <$> fa)
  {-# INLINE fmap #-}

instance Functor f => TermAlgebra (Free f) f where
  var = Var
  {-# INLINE var #-}
  con = Con
  {-# INLINE con #-}

data Void a
instance Functor Void where
  fmap f a = case a of {}
  {-# INLINE fmap #-}

runFree :: Free Void a -> a
runFree = fold undefined id

fold alg gen (Var x) = gen x
fold alg gen (Con op) = alg (fmap (fold alg gen) op)

instance (Monad m, TermAlgebra m f) => TermAlgebra (StateCarrier s m) (State s :+: f) where
  con = SC . (algState \/ conState) . fmap unSC
  {-# INLINE con #-}
  var = SC . genState
  {-# INLINE var #-}

conState :: (Functor g, TermAlgebra m g) => g (s -> m a) -> (s -> m a)
conState op s = con (fmap (\m -> m s) op)

(\/) :: (f b -> b) -> (g b -> b) -> ((f :+: g) b -> b)
(\/) algF algG (L1 s) = algF s
(\/) algF algG (R1 s) = algG s

run' :: Codensity (StateCarrier Int (Free Void)) a -> Int -> a
run' = fmap runFree . run

run :: (Monad m, TermAlgebra m f) => Codensity (StateCarrier s m) a -> s -> m a
run = unSC . flip runCodensity var

genH :: a -> H a
genH x = H $ \s -> (s, x)

algF :: F (H a) -> H a
algF (F f) = H $ \s -> case f s of (s', g) -> unH g s'

instance TermAlgebra f (State s :+: g) => MonadState s (Codensity f) where
  get = getState
  put s = putState s

-- generic API exactly as for MTL, only needed for msComputation
-- instance MonadState Int (Codensity H) where
--   {-# SPECIALIZE instance MonadState Int (Codensity H) #-}
--   get = wrap (F (\s -> (s, return s)))
--   put s = wrap (F (\_ -> (s, return ())))

instance MonadFree F (Codensity H) where
  wrap = alg_cod algF

instance TermAlgebra h f => TermAlgebra (Codensity h) f where
  var = return
  con = alg_cod con

alg_cod :: Functor f => (forall x. f (h x) -> h x) -> (f (Codensity h a) -> Codensity h a)
alg_cod alg = \op -> Codensity (\k -> alg (fmap (flip runCodensity k) op))

-- Could as well use Control.Monad.Copdensity from kan-extensions, except
-- that it has instances that overlap with the one for MonadState above.

newtype Codensity f a = Codensity {
  runCodensity :: forall b. (a -> f b) -> f b
}

instance Functor (Codensity f) where
  fmap f m = Codensity (\k -> runCodensity m (k. f))

instance Applicative (Codensity f) where
  pure = return
  (<*>) = ap

instance Monad (Codensity f) where
  return a = Codensity (\k -> k a)
  c >>= f  = Codensity (\k -> runCodensity c (\a -> runCodensity (f a) k))
