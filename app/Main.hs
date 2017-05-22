module Main where

import Constants (windowWidth, windowHeight, initialDoublePendulum, initialValueDiff, doublePendulumNumber)
import View (ColoredDoublePendulum(..), drawColoredDoublePendulum)

import DoublePendulum (DoublePendulum(..), DoublePendulumPosition(..), move)

import Graphics.Gloss

main :: IO ()
main = do
  let display = InWindow "Double Pendulum Simulation" (windowWidth, windowHeight) (10, 10)
      initialPendulums = replicate doublePendulumNumber initialDoublePendulum
      doublePendulums = flip map (zip [0..] initialPendulums)  $ \(i, dp) ->
        let pos = _pos dp
            th1 = _th1 pos
            th2 = _th2 pos
            thDiff = i * initialValueDiff * pi
         in dp {_pos = pos {_th1 = th1 + thDiff, _th2 = th2 + thDiff}}
      initialValue = flip map (zip [0..] doublePendulums) $ \(i, dp) ->
        ColoredDoublePendulum (greyN (1.0 - i / doublePendulumNumber)) dp
      drawColoredDoublePendulums = pictures . map drawColoredDoublePendulum
      moveColoredDoublePendulums h cdp = cdp {_dp = move h (_dp cdp)}
  simulate display white 30 initialValue drawColoredDoublePendulums (\_ h cdps -> map (moveColoredDoublePendulums (realToFrac h)) cdps)
