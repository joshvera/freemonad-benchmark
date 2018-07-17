A benchmark comparing the performance of different free monad implementations.

The benchmark simulates the state monad using various flavors of free monads,
and compares them to the standard State monad from transformers.

Note that this is *not* a comparison of extensible effects system. Free
monads *may* be used to implement an extensible effect system.
Under most implementations, extensible effects introduce an even bigger overhead
(dispatching upon the effect requests); this overhead is not present in this
benchmark. However, if your free monad is slow (which it probably is, as this
benchmark shows), any effect system based on it won't be fast.

## Running the benchmark

    stack build && stack exec freemonad-benchmark -- -o results.html

## Results

[Criterion report](https://rawgit.com/joshvera/freemonad-benchmark/master/results.html)

## Implementations

1. **Free**

    ``` haskell
    data Free f a = Pure a | Free (f (Free f a))
    ```

2. **Free/lazy**

    The same standard Free monad emulating the lazy State monad.

3. **Church**

    The Church-encoded free monad:

    ``` haskell
    newtype ChurchFree f a = ChurchFree
      { runChurchFree :: forall w. (a -> w) -> (f w -> w) -> w }
    ```

4. **Codensity**

    The standard Free monad, codensity-transformed. See
    [Asymptotic Improvement of Computations over Free Monads](http://www.janis-voigtlaender.eu/papers/AsymptoticImprovementOfComputationsOverFreeMonads.pdf).

5. **NoRemorse**

    A free monad from [Reflection without Remorse](http://okmij.org/ftp/Haskell/zseq.pdf).

6. **Freer**

    The Freer monad from [Freer Monads, More Extensible
    Effects](http://okmij.org/ftp/Haskell/extensible/more.pdf), aka the
    [operational]() monad.

7. **Effects**

    An extensible effects implementation with higher-order effects based off [Freer Monads, More Extensible Effects](http://okmij.org/ftp/Haskell/extensible/more.pdf) maintained by @joshvera and @robrix.

8. **Fused**

    An implementation of [Fusion for Free: Efficient
    Algebraic Effect Handlers](https://people.cs.kuleuven.be/~tom.schrijvers/Research/papers/mpc2015.pdf) by @brfk.

## Workloads

For every implementation, there are two tests, for left- and right-associated
chains of binds. Some free monads (e.g. the standard one) suffer from quadratic
complexity on left-associated chains of binds.
