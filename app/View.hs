module View (
  ColoredDoublePendulum(..),
  drawColoredDoublePendulum
  ) where

import DoublePendulum (DoublePendulum(..), DoublePendulumPosition(..))
import Graphics.Gloss

-- | 色付きの二重振り子
data ColoredDoublePendulum = ColoredDoublePendulum
  { _color :: Color
  , _dp    :: DoublePendulum
  }

-- | 振り子の長さの描画スケール
lengthScale :: Float
lengthScale = 100.0

-- | 振り子のおもりの描画スケール
massScale :: Float
massScale = 10.0

-- | ColoredDoublePendulum を描画する関数
drawColoredDoublePendulum :: ColoredDoublePendulum -> Picture
drawColoredDoublePendulum cdp =
  let dp    = _dp cdp
      l1    = lengthScale * realToFrac (_l1 dp)
      l2    = lengthScale * realToFrac (_l2 dp)
      m1    = massScale * sqrt (realToFrac (_m1 dp))
      m2    = massScale * sqrt (realToFrac (_m2 dp))
      pos   = _pos dp
      th1   = realToFrac (_th1 pos) - pi / 2.0
      th2   = realToFrac (_th2 pos) - pi / 2.0
      pt1@(pt1_x, pt1_y) = (l1 * cos th1, l1 * sin th1)
      (dpt_x, dpt_y)     = (l2 * cos th2, l2 * sin th2)
      pt2@(pt2_x, pt2_y) = (pt1_x + dpt_x, pt1_y + dpt_y)
      line1 = line [(0,0), pt1]
      line2 = line [pt1, pt2]
      circle1 = translate pt1_x pt1_y (circleSolid m1)
      circle2 = translate pt2_x pt2_y (circleSolid m2)
   in color (_color cdp) (pictures [line1, line2, circle1, circle2])

