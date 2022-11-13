{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# OPTIONS_GHC -Wno-type-defaults #-}

module CypherSpec (spec_cypher) where

import Cypher
import qualified Data.Map as M
import Data.Text
import Database.Bolt
import Test.Hspec

spec_cypher :: Spec
spec_cypher =
  describe "Cypher" $ do
    it "With single param" $
      runQuery ["MATCH (x{ name: " .> T "name" .> " })-[:F]->(y)"]
        `shouldBe` ( Data.Text.unlines ["MATCH (x{ name: $1 })-[:F]->(y)"],
                     M.singleton "1" (T $ pack "name")
                   )

    it "With two params" $
      runQuery ["MATCH (x{ name: " .> T "name" .> ", age: " .> I 18 .> " })-[:F]->(y)"]
        `shouldBe` ( Data.Text.unlines ["MATCH (x{ name: $1, age: $2 })-[:F]->(y)"],
                     M.fromList [("1", T $ pack "name"), ("2", I 18)]
                   )

    it "With multiple lines" $
      runQuery
        [ "MATCH (x{ name: " .> T "name" .> ", age: " .> I 18 .> " })-[:F]->(y)",
          "MATCH (y{ name: " .> T "yname" .> " })"
        ]
        `shouldBe` ( Data.Text.unlines
                       [ "MATCH (x{ name: $1, age: $2 })-[:F]->(y)",
                         "MATCH (y{ name: $3 })"
                       ],
                     M.fromList [("1", T $ pack "name"), ("2", I 18), ("3", T $ pack "yname")]
                   )

    it "With list of cyphers" $
      runQuery
        [ "MATCH (x{ name: " .> T "name" .> ", age: " .> I 18 .> " })-[:F]->(y)",
          "MATCH (y{ name: " .> T "yname" .> " })",
          qmap (\i -> ["MERGE (:Test { name: " .> I i .> " })-[:HAS_RESULT]->(:PASSED)"]) [1 .. 5]
        ]
        `shouldBe` ( Data.Text.unlines
                       [ "MATCH (x{ name: $1, age: $2 })-[:F]->(y)",
                         "MATCH (y{ name: $3 })",
                         "MERGE (:Test { name: $4 })-[:HAS_RESULT]->(:PASSED)",
                         "MERGE (:Test { name: $5 })-[:HAS_RESULT]->(:PASSED)",
                         "MERGE (:Test { name: $6 })-[:HAS_RESULT]->(:PASSED)",
                         "MERGE (:Test { name: $7 })-[:HAS_RESULT]->(:PASSED)",
                         "MERGE (:Test { name: $8 })-[:HAS_RESULT]->(:PASSED)",
                         ""
                       ],
                     M.fromList [("1", T $ pack "name"), ("2", I 18), ("3", T $ pack "yname"), ("4", I 1), ("5", I 2), ("6", I 3), ("7", I 4), ("8", I 5)]
                   )
