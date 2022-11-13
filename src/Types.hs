{-# LANGUAGE DeriveGeneric #-}

module Types (Player (..), Gender (..), DoublesPair, Match (..), Score, ClubId (..), ReadyMatch (..)) where

import Data.Text (Text)
import GHC.Generics

data Player = Player
  { playerId :: Int,
    playerName :: Text,
    playerSingleRank :: Int,
    playerDoubleRank :: Int,
    playerMixedRank :: Int,
    playerGender :: Gender
  }
  deriving (Show, Eq, Generic)

data Gender = Man | Woman
  deriving (Show, Eq, Generic)

type DoublesPair = (Player, Player)

data ReadyMatch = ReadyMatch Match Score
  deriving (Show, Eq, Generic)

data Match
  = Singles Player Player
  | Doubles DoublesPair DoublesPair
  | Mixed DoublesPair DoublesPair
  deriving (Show, Eq, Generic)

type Score = (Int, Int)

newtype ClubId = ClubId {club :: Text}