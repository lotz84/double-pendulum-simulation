{-# LANGUAGE TypeFamilies #-}

module DoublePendulum (
  DoublePendulum(..),
  DoublePendulumPosition(..),
  move
  ) where

import Data.VectorSpace (AdditiveGroup(..), VectorSpace(..))
import RungeKutta (rk4)

-- | 二重振り子
data DoublePendulum = DoublePendulum
  { _l1   :: Double -- 一つ目の振り子の長さ
  , _m1   :: Double -- 一つ目の振り子の質量
  , _l2   :: Double -- 二つ目の振り子の長さ
  , _m2   :: Double -- 二つ目の振り子の質量
  , _pos  :: DoublePendulumPosition
  } deriving Show

-- | 二重振り子の角度と角速度
data DoublePendulumPosition = DoublePendulumPosition
  { _th1   :: Double -- 一つ目の振り子の角度
  , _dth1  :: Double -- 一つ目の振り子の角速度
  , _th2   :: Double -- 二つ目の振り子の角度
  , _dth2  :: Double -- 二つ目の振り子の角速度
  } deriving Show

instance AdditiveGroup DoublePendulumPosition where
  zeroV = DoublePendulumPosition 0.0 0.0 0.0 0.0
  (DoublePendulumPosition th1_1 dth1_1 th2_1 dth2_1) ^+^ (DoublePendulumPosition th1_2 dth1_2 th2_2 dth2_2) =
    DoublePendulumPosition (th1_1 + th1_2) (dth1_1 + dth1_2) (th2_1 + th2_2) (dth2_1 + dth2_2)
  negateV (DoublePendulumPosition th1 dth1 th2 dth2) = DoublePendulumPosition (-th1) (-dth1) (-th2) (-dth2)

instance VectorSpace DoublePendulumPosition where
  type Scalar DoublePendulumPosition = Double
  a *^ (DoublePendulumPosition th1 dth1 th2 dth2) = DoublePendulumPosition (a * th1) (a * dth1) (a * th2) (a * dth2)

-- | 二重振り子の運動方程式
eom :: DoublePendulum -> DoublePendulumPosition
eom dp =
  let g   = 9.8
      l1  = _l1 dp
      l2  = _l2 dp
      l   = l1 / l2
      mu  = 1.0 + _m1 dp / _m2 dp
      pos = _pos dp
      th1 = _th1 pos
      th2 = _th2 pos
      dth1 = _dth1 pos
      dth2 = _dth2 pos
      dth = th1 - th2
      denom = mu - (cos dth) ^ 2
      ddth1 = ((g / l1) * (sin th2 * cos dth - mu * sin th1) - (dth2 ^ 2 / l + dth1 ^ 2 * cos dth) * sin dth) / denom
      ddth2 = ((g * mu / l2) * (sin th1 * cos dth - sin th2) + (mu * l * dth1 ^ 2 + dth2 ^ 2 * cos dth) * sin dth) / denom
   in DoublePendulumPosition dth1 ddth1 dth2 ddth2

-- | 二重振り子の状態を少しだけ進める関数
move :: Double -> DoublePendulum -> DoublePendulum
move h dp =
  let pos = rk4 h (\(pos, _) -> eom (dp {_pos = pos})) (_pos dp, 0)
   in dp {_pos = pos}
