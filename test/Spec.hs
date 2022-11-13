module Spec (main, spec_getHandicap) where

import CypherSpec
import RankerSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  spec_getHandicap
  spec_cypher
