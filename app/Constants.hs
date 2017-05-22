module Constants (
  windowWidth,
  windowHeight,
  initialDoublePendulum,
  initialValueDiff,
  doublePendulumNumber
  ) where

import DoublePendulum (DoublePendulum(..), DoublePendulumPosition(..))

-- | ウィンドウの幅
windowWidth :: Num a => a
windowWidth = 600

-- | ウィンドウの高さ
windowHeight :: Num a => a
windowHeight = 600

-- | 二重振り子の初期値
initialDoublePendulum = DoublePendulum
  { _l1 = 1.0
  , _m1 = 1.0
  , _l2 = 1.0
  , _m2 = 1.0
  , _pos = DoublePendulumPosition
    { _th1  = 0.75 * pi
    , _dth1 = 0.0
    , _th2  = 0.75 * pi
    , _dth2 = 0.0
    }
  }

-- | 初期値の差
initialValueDiff :: Fractional a => a
initialValueDiff = 1e-12

-- | 二重振り子の数
doublePendulumNumber :: Num a => a
doublePendulumNumber = 50
