{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Ranker (getHandicap) where

import Types

getHandicap :: Match -> Score
getHandicap match = setHandicapSide (rankDiff match)

setHandicapSide :: Int -> Score
setHandicapSide handicap
  | handicap < 0 = (min 10 (handicap * (-1)), 0)
  | otherwise = (0, min 10 handicap)

rankDiff :: Match -> Int
rankDiff (Singles p1 p2) = truncate $ fromIntegral (playerSingleRank p1 - playerSingleRank p2) / 20
rankDiff (Doubles (pa1, pa2) (pb1, pb2)) = truncate $ fromIntegral (playerDoubleRank pa1 + playerDoubleRank pa2 - playerDoubleRank pb1 - playerDoubleRank pb2) / 40
rankDiff (Mixed (pa1, pa2) (pb1, pb2)) = truncate $ fromIntegral (playerMixedRank pa1 + playerMixedRank pa2 - playerMixedRank pb1 - playerMixedRank pb2) / 40