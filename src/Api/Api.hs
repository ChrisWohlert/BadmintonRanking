{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Api (runApi) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Swagger
import Database.Bolt
import GHC.Generics
import Infrastructure
import Network.Wai
import Network.Wai.Handler.Warp
import Ranker
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import Store
import System.IO
import Types

type AppHandler = ReaderT Env Handler

-- * api

data TeamReqest = TeamReqest {playerId1 :: Int, playerId2 :: Int}
  deriving (Show, Eq, Generic)

data StartMatchRequest
  = SinglesRequest {pid1 :: Int, pid2 :: Int}
  | DoublesRequest {dteam1 :: TeamReqest, dteam2 :: TeamReqest}
  | MixedRequest {mteam1 :: TeamReqest, mteam2 :: TeamReqest}
  deriving (Show, Eq, Generic)

type RestApi =
  "players" :> Get '[JSON] [Player]
    :<|> "startMatch" :> ReqBody '[JSON] StartMatchRequest :> Post '[JSON] ReadyMatch

proxyApi :: Proxy Api
proxyApi = Proxy

type Api =
  SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> RestApi

-- * app

runApi :: IO ()
runApi = do
  let port = 3000
      settings =
        setPort port $
          setBeforeMainLoop
            (hPutStrLn stderr ("listening on port " ++ show port))
            defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve proxyApi server

server :: Server Api
server = hoistServer (Proxy :: Proxy Api) (`runReaderT` env) api :: Server Api

api :: ServerT Api AppHandler
api =
  swaggerSchemaUIServerT swaggerDoc
    :<|> getPlayers
    :<|> startMatch

getPlayers :: AppHandler [Player]
getPlayers = inPipe $ readAllPlayers (ClubId "Vejlby IK")

startMatch :: StartMatchRequest -> AppHandler ReadyMatch
startMatch request = do
  ps <- inPipe (readPlayers getPlayersFromRequest)
  m <- getMatchFromRequest ps
  let h = getHandicap m
  return $ ReadyMatch m h
  where
    getPlayersFromRequest = case request of
      (SinglesRequest p1 p2) -> [p1, p2]
      (DoublesRequest (TeamReqest pa1 pa2) (TeamReqest pb1 pb2)) -> [pa1, pa2, pb1, pb2]
      (MixedRequest (TeamReqest pa1 pa2) (TeamReqest pb1 pb2)) -> [pa1, pa2, pb1, pb2]

    getMatchFromRequest ps = case (request, ps) of
      (SinglesRequest _ _, [p1, p2]) -> pure $ Singles p1 p2
      (DoublesRequest _ _, [p1, p2, p3, p4]) -> pure $ Doubles (p1, p2) (p3, p4)
      (MixedRequest _ _, [p1, p2, p3, p4]) -> pure $ Mixed (p1, p2) (p3, p4)
      _ -> throwError err500

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

instance ToJSON StartMatchRequest

instance FromJSON StartMatchRequest

instance ToJSON TeamReqest

instance FromJSON TeamReqest

-- * Swagger

instance ToSchema Player

instance ToSchema Gender

instance ToSchema Match

instance ToSchema ReadyMatch

instance ToSchema StartMatchRequest

instance ToSchema TeamReqest

swaggerDoc :: Swagger
swaggerDoc =
  toSwagger (Proxy :: Proxy RestApi)