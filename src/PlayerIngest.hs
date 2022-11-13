{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module PlayerIngest
  ( playerIngest,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class
import Data.Either (rights)
import Data.Functor
import Data.Text hiding (drop, filter, length, take)
import Database.Bolt
import Debug.Trace
import GHC.IO.Encoding
import Scraper
import Store
import Types
import Web.Api.WebDriver
import qualified Web.Api.WebDriver.Types.Keyboard as Keys

playerIngest :: Pipe -> IO ()
playerIngest pipe = do
  threadDelay (1000 * 3000)
  setLocaleEncoding utf8
  (Right (players, clubName), _, _) <-
    execWebDriverT
      conf
      (runIsolated defaultChromeCapabilities execPlayers)
  savePlayers players clubName pipe
  putStrLn "Players ingested"

execPlayers :: WebDriver ([Player], Text)
execPlayers = do
  liftIO $ threadDelay (1000 * 3000)
  breakpointsOn
  catchError
    ( do
        liftIO $ setLocaleEncoding utf8
        navigateTo "https://www.badmintonplayer.dk/DBF/Ranglister/"
        void maximizeWindow
        let clubName = "Vejlby IK"
        "#TextBoxName" |>= elementSendKeys (" Vejlby IK" <> singleton (keyToChar Keys.HomeKey) <> singleton (keyToChar Keys.DeleteKey) <> singleton (keyToChar Keys.EnterKey))
        ".rankingstabs > div:nth-child(4) > a" |>= click
        men <- getPlayers Man
        ".rankingstabs > div:nth-child(5) > a" |>= click
        women <- getPlayers Woman
        let players = men ++ women
        liftIO $ print players
        return (players, clubName)
    )
    (\e -> breakpoint (pack $ show e) >> traceShowM "ERROR" >> return ([], ""))
  where
    getPlayers gender = do
      playerRows <- ".RankingListGrid > tbody > tr:not(:first-child)" |>> getPlayerRow
      liftIO . print $ "Got rows: " <> show (length $ rights playerRows)
      traverse (getPlayer gender) (rights playerRows)

    getPlayer gender (pid, pname, url) = do
      traceShowM (pid, pname, url)
      navigateTo $ "https://www.badmintonplayer.dk" <> url
      ".playerprofiletabscontainer > div:nth-child(1) > a" |>= click
      srank <- getRank ".playerprofilerankingpointstable tr:nth-child(2) > td:nth-child(3)"
      drank <- getRank ".playerprofilerankingpointstable tr:nth-child(3) > td:nth-child(3)"
      mrank <- getRank ".playerprofilerankingpointstable tr:nth-child(4) > td:nth-child(3)"
      liftIO $ print (pid, pname, srank, drank, mrank)
      back
      back
      return $ Player pid pname srank drank mrank gender

    getRank selector =
      read
        . unpack
        <$> selector
        |>= getText
        `onTimeout` return "0"

getPlayerRow :: ElementRef -> WebDriver (Either Text (Int, Text, Text))
getPlayerRow row = do
  pid <- row |-> ".name > a" >>= getElementAttribute "href" <&> (getAttrValue "Missing pid" >=> return . read . unpack . Data.Text.reverse . Data.Text.takeWhile (/= '#') . Data.Text.reverse)
  pname <- row |-> ".name" >>= getText
  url <- (row |-> ".points > a" >>= getElementAttribute "href") <&> getAttrValue "Missing name"
  return $ (,Data.Text.takeWhile (/= ',') pname,) <$> pid <*> url

getAttrValue :: Text -> Either Bool Text -> Either Text Text
getAttrValue t (Left _) = Left t
getAttrValue _ (Right h) = Right h

conf :: WebDriverConfig IO
conf = setToChrome defaultWebDriverConfig

setToChrome :: WebDriverConfig eff -> WebDriverConfig eff
setToChrome (WDConfig s r eva) =
  WDConfig
    s
    ( r
        { _env =
            defaultWDEnv
              { _remotePort = 9515
              }
        }
    )
    eva