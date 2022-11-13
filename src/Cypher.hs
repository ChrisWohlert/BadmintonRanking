{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cypher (Cypher (..), ToCypher (..), (.>), (.>>), runQuery, qmap, unwind) where

import Control.Monad.State.Lazy
import Data.List (intersperse)
import qualified Data.Map as M
import Data.String
import qualified Data.Text as T
import Database.Bolt

data Cypher
  = CText String
  | CValue Value
  | CList [Cypher]

instance Semigroup Cypher where
  a <> b = CList [a, b]

class ToCypher a where
  toCypher :: a -> Cypher

instance IsString Cypher where
  fromString = CText

instance ToCypher Cypher where
  toCypher = id

instance (ToCypher a) => ToCypher [a] where
  toCypher as = CList (map toCypher as)

instance ToCypher Value where
  toCypher = CValue

instance ToCypher Char where
  toCypher c = CText [c]

instance ToCypher T.Text where
  toCypher = CText . T.unpack

(.>) :: (ToCypher a, ToCypher b) => a -> b -> Cypher
t .> v = CList [toCypher t, toCypher v]

infixl 1 .>

(.>>) :: (Monad m, RecordValue a) => T.Text -> (a -> BoltActionT m b) -> Record -> BoltActionT m b
a .>> b = \r -> r `at` a >>= b

runQuery :: [Cypher] -> (T.Text, M.Map T.Text Value)
runQuery = (,) <$> genQuery <*> genMap

genQuery :: [Cypher] -> T.Text
genQuery cs = T.pack . unlines $ evalState (traverse gen cs) 0
  where
    gen :: Cypher -> State Int String
    gen (CText t) = pure t
    gen (CValue _) = do
      modify (+ 1)
      i <- get
      pure $ "$" <> show i
    gen (CList xs) = mconcat <$> traverse gen xs

genMap :: [Cypher] -> M.Map T.Text Value
genMap cs = mconcat $ evalState (traverse gen cs) 0
  where
    gen :: Cypher -> State Int (M.Map T.Text Value)
    gen (CText _) = pure mempty
    gen (CValue v) = do
      modify (+ 1)
      i <- get
      pure $ M.singleton (T.pack (show i)) v
    gen (CList xs) = mconcat <$> traverse gen xs

qmap :: (a -> [Cypher]) -> [a] -> Cypher
qmap f xs = (CList . map (.> "\n")) (concatMap f xs)

unwind :: T.Text -> (a -> [Cypher]) -> [a] -> Cypher
unwind name f xs = "UNWIND [" .> CList (intersperse (CText ", ") (map (wrapWithBrackets . f) xs)) .> "] as " <> T.unpack name

wrapWithBrackets :: [Cypher] -> Cypher
wrapWithBrackets xs = "{ " .> intersperse (CText ", ") xs .> " }"
