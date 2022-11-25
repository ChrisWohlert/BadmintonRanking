{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Api.Api (runApi, swaggerDoc) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Swagger
import Database.Bolt
import GHC.Generics
import Infrastructure
import Network.Wai
import Network.Wai.Handler.Warp
import PlayerIngest (playerIngest)
import Ranker
import Servant
import Servant.API.Generic
import Servant.Server.Generic (AsServerT, genericServeT)
import Servant.Swagger
import Servant.Swagger.UI
import Store
import System.IO
import Types

type AppHandler = ReaderT Env Handler

-- * api

data DoublesRequest = DoublesRequest {playerId1 :: Int, playerId2 :: Int, playerId3 :: Int, playerId4 :: Int}
  deriving (Show, Eq, Generic)

data MixedRequest = MixedRequest {playerId1 :: Int, playerId2 :: Int, playerId3 :: Int, playerId4 :: Int}
  deriving (Show, Eq, Generic)

data SinglesRequest = SinglesRequest {playerId1 :: Int, playerId2 :: Int}
  deriving (Show, Eq, Generic)

data RestApi route = RestApi
  { _players :: route :- "players" :> Get '[JSON] [Player],
    _singleStart :: route :- "single" :> "start" :> ReqBody '[JSON] SinglesRequest :> Post '[JSON] ReadyMatch,
    _doubleStart :: route :- "double" :> "start" :> ReqBody '[JSON] DoublesRequest :> Post '[JSON] ReadyMatch,
    _mixedStart :: route :- "mixed" :> "start" :> ReqBody '[JSON] MixedRequest :> Post '[JSON] ReadyMatch,
    _ingestPlayers :: route :- "ingest" :> "players" :> Post '[JSON] String
  }
  deriving (Generic)

data Api route = Api
  { _swagger :: route :- SwaggerSchemaUI "swagger-ui" "swagger.json",
    _api :: route :- NamedRoutes RestApi
  }
  deriving (Generic)

instance HasSwagger (ToServantApi routes) => HasSwagger (NamedRoutes routes) where
  toSwagger _ = toSwagger (Proxy :: Proxy (ToServantApi routes))

-- * app

runApi :: IO ()
runApi = do
  let p = 3000
      settings =
        setPort p $
          setBeforeMainLoop
            (hPutStrLn stderr ("listening on port " ++ show p))
            defaultSettings
  runSettings settings mkApp

recordRoute :: Api (AsServerT AppHandler)
recordRoute =
  Api
    { _swagger = swaggerSchemaUIServerT swaggerDoc,
      _api =
        RestApi
          { _players = getPlayers,
            _singleStart = singleStart,
            _doubleStart = doubleStart,
            _mixedStart = mixedStart,
            _ingestPlayers = inPipe playerIngest >> return "Done ingesting"
          }
    }

mkApp :: Application
mkApp = genericServeT (`runReaderT` env) recordRoute

getPlayers :: AppHandler [Player]
getPlayers = inPipe $ readAllPlayers (ClubId "Vejlby IK")

singleStart :: SinglesRequest -> AppHandler ReadyMatch
singleStart (SinglesRequest p1 p2) =
  startMatch [p1, p2] $
    \case
      [x1, x2] -> pure $ Singles x1 x2
      _ -> throwError err400

doubleStart :: DoublesRequest -> AppHandler ReadyMatch
doubleStart (DoublesRequest p1 p2 p3 p4) = startMatch [p1, p2, p3, p4] $
  \case
    [x1, x2, x3, x4] -> pure $ Doubles (x1, x2) (x3, x4)
    _ -> throwError err400

mixedStart :: MixedRequest -> AppHandler ReadyMatch
mixedStart (MixedRequest p1 p2 p3 p4) = startMatch [p1, p2, p3, p4] $
  \case
    [x1, x2, x3, x4] -> pure $ Mixed (x1, x2) (x3, x4)
    _ -> throwError err400

startMatch :: [Int] -> ([Player] -> ReaderT Env Handler Match) -> ReaderT Env Handler ReadyMatch
startMatch players getMatchFromRequest = do
  ps <- inPipe (readPlayers players)
  m <- getMatchFromRequest ps
  let h = getHandicap m
  return $ ReadyMatch m h

inPipe :: (Pipe -> IO a) -> AppHandler a
inPipe a = ask >>= \e -> liftIO $ withDb e a

-- * Json

instance ToJSON Player

instance FromJSON Player

instance ToJSON Gender

instance FromJSON Gender

instance ToJSON Match

instance FromJSON Match

instance ToJSON ReadyMatch

instance FromJSON ReadyMatch

instance ToJSON DoublesRequest

instance FromJSON DoublesRequest

instance ToJSON MixedRequest

instance FromJSON MixedRequest

instance ToJSON SinglesRequest

instance FromJSON SinglesRequest

-- * Swagger

instance ToSchema Player

instance ToSchema Gender

instance ToSchema Match

instance ToSchema ReadyMatch

instance ToSchema SinglesRequest

instance ToSchema DoublesRequest

instance ToSchema MixedRequest

swaggerDoc :: Swagger
swaggerDoc =
  toSwagger (Proxy :: Proxy (ToServantApi RestApi))