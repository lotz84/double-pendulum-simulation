{-# LANGUAGE FlexibleContexts #-}

module RungeKutta (
  rk4
  ) where

import Data.VectorSpace (VectorSpace(..), (^+^))

-- | 4次のルンゲクッタ法
rk4 :: (VectorSpace x, Fractional (Scalar x)) => Scalar x -> ((x, Scalar x) -> x) -> (x, Scalar x) -> x
rk4 h f (x, t) =
  let k1 = f (x, t)
      k2 = f (x ^+^ (h / 2.0) *^ k1, t + (h / 2.0))
      k3 = f (x ^+^ (h / 2.0) *^ k2, t + (h / 2.0))
      k4 = f (x ^+^ h *^ k3, t + h)
   in x ^+^ (h / 6.0) *^ (k1 ^+^ 2.0 *^ k2 ^+^ 2.0 *^ k3 ^+^ k4)

