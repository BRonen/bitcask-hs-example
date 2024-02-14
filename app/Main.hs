{-# LANGUAGE DerivingVia, DeriveGeneric #-}

import qualified Bitcask as BC
import qualified Data.ByteString.Lazy.UTF8 as BLU

import Entry
import Network.RPC.Curryer.Server
import Codec.Winery
import GHC.Generics

data SetKey = SetKey String String
  deriving (Generic, Show)
  deriving Serialise via WineryVariant SetKey

data GetKey = GetKey String
  deriving (Generic, Show)
  deriving Serialise via WineryVariant GetKey

main :: IO ()
main = do
  handle' <- BC.open "tmp/" True
  print handle'
  case handle' of
    Left err -> print err
    Right handle -> do
      BC.listKeys handle >>= print
      _ <- serve (kvRequestHandlers handle) () localHostAddr 8765 Nothing
      pure ()

kvRequestHandlers :: BC.Handle -> RequestHandlers ()
kvRequestHandlers handle = [ RequestHandler $ \_ (SetKey k v) -> do
                        t <- BC.put handle (BLU.fromString k) (BLU.fromString v)
                        case t of
                          Right entry@(Entry _ _ _ _ _ t') -> do
                            print entry
                            pure $ BLU.toString t'
                          Left err -> pure err
                    , RequestHandler $ \_ (GetKey k) -> do
                        value <- BC.get handle (BLU.fromString k)
                        case value of
                          Just value' -> do
                            print value'
                            pure $ Just $ BLU.toString value'
                          Nothing -> do
                            pure $ Nothing
                    ]
