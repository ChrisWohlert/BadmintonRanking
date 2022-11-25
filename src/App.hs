module App
  ( app,
    swag
  )
where

-- import PlayerIngest
import Api.Api
import System.IO
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Aeson

app :: IO ()
app = do
  mapM_ makeSafe [stdout, stdin, stderr]
  -- playerIngest
  runApi

makeSafe h = do
  ce' <- hGetEncoding h
  case ce' of
    Nothing -> return ()
    Just ce ->
      mkTextEncoding (takeWhile (/= '/') (show ce) ++ "//TRANSLIT")
        >>= hSetEncoding h


swag = BL8.writeFile "swagger.json" (encode swaggerDoc)