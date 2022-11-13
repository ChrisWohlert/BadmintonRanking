{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Store (savePlayers, readAllPlayers, readPlayers) where

import Control.Monad
import Control.Monad.Cont
import Cypher
import Data.Text
import Database.Bolt
import Debug.Trace (traceShowM)
import Types

execAll :: MonadIO m => (a -> [Cypher]) -> [a] -> Pipe -> m ()
execAll q a pipe = void $ run pipe $ traverse (uncurry queryP . runQuery . q) a

runOne :: MonadIO m => [Cypher] -> Pipe -> m ()
runOne q pipe = void $ run pipe $ uncurry queryP . runQuery $ q

readOne :: MonadIO m => [Cypher] -> (Record -> BoltActionT m a) -> Pipe -> m (Maybe a)
readOne q f pipe =
  readAll q f pipe >>= \case
    [x] -> pure $ Just x
    _ -> pure Nothing

readAll :: MonadIO m => [Cypher] -> (Record -> BoltActionT m a) -> Pipe -> m [a]
readAll q f pipe = run pipe . (uncurry queryP >=> traverse f) . runQuery $ q

savePlayers :: [Player] -> Text -> Pipe -> IO ()
savePlayers players clubName = execAll (insertPlayerQuery clubName) players

insertPlayerQuery :: Text -> Player -> [Cypher]
insertPlayerQuery clubName (Player pid pname srank drank mrank gender) =
  [ "MERGE (c:Club {clubName: " .> T clubName .> "})",
    "MERGE (p:Player {pid: " .> I pid .> "})",
    "MERGE (c)-[:HasMember]->(p)",
    "SET p.name = " .> T pname,
    "SET p.singleRank = " .> I srank,
    "SET p.doubleRank = " .> I drank,
    "SET p.mixedRank = " .> I mrank,
    "SET p.gender = " .> T (pack $ show gender),
    "RETURN p"
  ]

readAllPlayers :: MonadIO m => ClubId -> Pipe -> m [Player]
readAllPlayers (ClubId clubName) =
  readAll
    [ "MATCH (n:Club { clubName: " .> T clubName .> " })-[HasMemeber]->(p:Player) ",
      "RETURN p{.*} as players"
    ]
    ("players" .>> mapToPlayer)

mapToPlayer :: Monad m => Record -> BoltActionT m Player
mapToPlayer r =
  Player
    <$> r `at` "pid"
    <*> r `at` "name"
    <*> r `at` "singleRank"
    <*> r `at` "doubleRank"
    <*> r `at` "mixedRank"
    <*> r `at` "gender"

readPlayers :: MonadIO m => [Int] -> Pipe -> m [Player]
readPlayers pids =
  readAll
    [ "(p:Player)",
      "WHERE p.playerId IN " .> Prelude.map I pids,
      "RETURN p{.*} as players"
    ]
    ("players" .>> mapToPlayer)

instance RecordValue Gender where
  exactEither (T gender) = case gender of
    "Man" -> pure Man
    "Woman" -> pure Woman
    _ -> Left NotString
  exactEither _ = Left $ Not "Man | Woman"