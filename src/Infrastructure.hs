{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Infrastructure (Env (..), env) where

import Data.Default
import Database.Bolt hiding (version)

data Env = Env
  { withDb :: forall a. (Pipe -> IO a) -> IO a,
    version :: Int
  }

dbConfig :: BoltCfg
dbConfig = def {user = "neo4j", password = "1234", host = "localhost", port = 7687}

env =
  Env
    { withDb = \f -> do
        pipe <- connect dbConfig
        a <- f pipe
        close pipe
        return a,
      version = 0
    }