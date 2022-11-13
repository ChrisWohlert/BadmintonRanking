{-# LANGUAGE OverloadedStrings #-}

module RankerSpec (spec_getHandicap) where

import Control.Monad
import Data.Text
import Ranker
import Test.Hspec
import Text.Printf
import Types

spec_getHandicap :: Spec
spec_getHandicap = do
  describe "getHandicap" $ do
    forM_
      [ (1000, 1100, (5, 0)),
        (1100, 1000, (0, 5)),
        (1000, 1000, (0, 0)),
        (1000, 1050, (2, 0)),
        (1000, 1200, (10, 0)),
        (1000, 1400, (10, 0))
      ]
      $ \(p1, p2, r) ->
        it (printf "singles: given '%i' and '%i', returns '%s'" p1 p2 (show r)) $
          getHandicap (Singles (singlesPlayer p1) (singlesPlayer p2)) `shouldBe` r

    forM_
      [ (1000, 1000, 1000, 1100, (2, 0)),
        (1100, 1000, 1000, 1000, (0, 2)),
        (1000, 1000, 1000, 1000, (0, 0)),
        (1000, 1000, 1000, 1200, (5, 0)),
        (1000, 1000, 1000, 1400, (10, 0)),
        (1000, 1000, 1000, 1800, (10, 0))
      ]
      $ \(pa1, pa2, pb1, pb2, r) ->
        it (printf "doubles: given ('%i', '%i') and ('%i', '%i'), returns '%s'" pa1 pa2 pb1 pb2 (show r)) $
          getHandicap (Doubles (doublesPlayer pa1, doublesPlayer pa2) (doublesPlayer pb1, doublesPlayer pb2)) `shouldBe` r

    forM_
      [ (1000, 1000, 1000, 1100, (2, 0)),
        (1100, 1000, 1000, 1000, (0, 2)),
        (1000, 1000, 1000, 1000, (0, 0)),
        (1000, 1000, 1000, 1200, (5, 0)),
        (1000, 1000, 1000, 1400, (10, 0)),
        (1000, 1000, 1000, 1800, (10, 0))
      ]
      $ \(pa1, pa2, pb1, pb2, r) ->
        it (printf "mixed: given ('%i', '%i') and ('%i', '%i'), returns '%s'" pa1 pa2 pb1 pb2 (show r)) $
          getHandicap (Mixed (mixedPlayer pa1, mixedPlayer pa2) (mixedPlayer pb1, mixedPlayer pb2)) `shouldBe` r

singlesPlayer :: Int -> Player
singlesPlayer rank = Player 0 (pack $ show rank <> " rank player") rank 0 0 Woman

doublesPlayer :: Int -> Player
doublesPlayer rank = Player 0 (pack $ show rank <> " rank player") 0 rank 0 Woman

mixedPlayer :: Int -> Player
mixedPlayer rank = Player 0 (pack $ show rank <> " rank player") 0 0 rank Woman