{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Scraper
  ( elemWithText,
    ifExists,
    (|>=),
    (|->),
    (|-->),
    (|><),
    (|>>),
    WebDriver,
    click,
    getText,
    findElem,
    findElems,
    refresh,
    back,
    findElemFrom,
    findElemsFrom,
    waitUntil,
    onTimeout,
  )
where

import Control.Monad
import Data.Functor
import Data.String hiding (words)
import Data.Text hiding (drop, filter, length, map, take, zip)
import Debug.Trace
import Web.Api.WebDriver

type WebDriver m = WebDriverT IO m

type Element = ElementRef

type WebElement = WebDriver Element

type WebElements = WebDriver [Element]

instance IsString WebElement where
  fromString :: String -> WebDriver Element
  fromString = findElement CssSelector . pack

instance IsString WebElements where
  fromString :: String -> WebDriver [Element]
  fromString = findElements CssSelector . pack

elemWithText :: Selector -> Text -> WebDriver Element
elemWithText selector t = waitUntil 15 $ do
  es <- findElems selector
  filtered <- catchError (filterM (fmap (t `isPrefixOf`) . getText) es) (const $ return [])
  case filtered of
    [] -> throwError NoSession
    (x : _) -> return x

ifExists :: Selector -> a -> WebDriver a -> WebDriver a
ifExists selector d f = waitUntil 5 (findElem selector $> d) `onTimeout` f

(|>=) :: WebDriver Element -> (Element -> WebDriver b) -> WebDriver b
selector |>= f = waitUntil 15 selector >>= f

(|->) :: Element -> Selector -> WebDriver Element
element |-> selector = findElemFrom element selector

(|-->) :: Element -> Selector -> WebDriver [Element]
element |--> selector = findElemsFrom element selector

(|><) :: WebDriver Element -> WebDriver a -> WebDriver a
e |>< f = do
  e |>= click
  y <- waitUntil 15 f
  back
  refresh
  return y

infixr 1 |><

(|>>) :: Selector -> (Element -> WebDriver a) -> WebDriver [a]
gen |>> f = loop gen f 0
  where
    loop :: Selector -> (Element -> WebDriver a) -> Int -> WebDriver [a]
    loop g f' offset = do
      refresh
      es <- waitUntil 30 (findElem g *> findElems g)
      case drop offset es of
        (n : _) -> do
          t <- getText n
          traceShowM t
          r <- f' n
          rest <- waitUntil 15 $ loop g f' (offset + 1)
          return $ r : rest
        [] -> return []

click = elementClick

getText = getElementText

findElem = findElement CssSelector

findElems = findElements CssSelector

refresh = pageRefresh

back = goBack

findElemFrom e s = findElementFromElement CssSelector s e

findElemsFrom e s = findElementsFromElement CssSelector s e

waitUntil 0 f = do
  f
waitUntil i f = do
  catchError
    f
    (\x -> traceShowM ("Waiting for: " <> show x) >> wait 1000 >> waitUntil (i - 1) f)

f `onTimeout` g =
  catchError
    f
    (const g)
