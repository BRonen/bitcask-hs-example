{-# LANGUAGE DerivingVia, DeriveGeneric, OverloadedStrings #-}
import Network.RPC.Curryer.Client
import Network.RPC.Curryer.Server (localHostAddr)
import Control.Concurrent
import Options.Generic
import Codec.Winery

data SetKey = SetKey String String
  deriving (Generic, Show)
  deriving Serialise via WineryVariant SetKey

data GetKey = GetKey String
  deriving (Generic, Show)
  deriving Serialise via WineryVariant GetKey

set :: String -> String -> IO ()
set k v = do
  threadDelay 500000
  conn <- connect [] localHostAddr 8765
  eRet <- call conn (SetKey k v)
  case eRet of
    Left err -> error (show err)
    Right () -> do
      putStrLn $ "updated key " ++ k
      pure ()

get :: String -> IO ()
get k = do
  threadDelay 500000
  conn <- connect [] localHostAddr 8765
  eRet <- call conn (GetKey k)
  case eRet of
    Left err -> error (show err)
    Right (Just val) -> putStrLn val
    Right Nothing -> putStrLn "no such key"


main :: IO ()
main = do
  get "hello"
  set "hello" "world"
  get "hello"
  get "hello 1"
  set "hello 1" "world 1"
  get "hello 1"
  set "hello" "bananas"
  get "hello"