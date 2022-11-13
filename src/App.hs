module App
  ( app,
  )
where

-- import PlayerIngest
import Api.Api
import System.IO

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
