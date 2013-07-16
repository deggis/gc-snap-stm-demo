{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module KVBackend (
    initKVBackend
   ,KVBackend ) where

import Snap.Snaplet
import Snap.Core
import Control.Monad.IO.Class
import Control.Applicative

import Data.Text.Encoding
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)

import KeyValueStorage

data KVBackend = KVBackend Storage

type KVHandler r = forall a. Handler a KVBackend r

get :: Storage -> KVHandler ()
get s = do
--  (KVBackend s) <- get
  key <- fmap decodeUtf8 <$> getParam "key"
  case key of
      Just k  -> do
          mbDoc <- liftIO $ getDoc s k
          case mbDoc of
              Just doc -> writeBS . toStrict $ doc
              _        -> bad "Not found"
      Nothing -> bad "No key"

put :: Storage -> KVHandler ()
put s = do
  mbKey <- fmap decodeUtf8 <$> getParam "key"
  value <- getRequestBody
  case mbKey of
    Just key -> do
      overwritten <- liftIO $ putDoc s key value
      good $ if overwritten then "Overwritten" else "Created"
    Nothing  -> bad "Key missing"


bad,good :: ByteString -> KVHandler ()
bad msg  = modifyResponse (setResponseStatus 400 "Bad request") >> writeBS msg
good msg = writeBS msg

routes a = [("/hello", writeText "hello world with snap")
         ,("/doc/:key",  method GET $ get a)
         ,("/doc/:key",  method PUT $ put a)
         ]

initKVBackend :: SnapletInit a KVBackend
initKVBackend = makeSnaplet "KVBackend" "STM dojo web with Snaplet" Nothing $ do
    storage <- liftIO initStorage
    addRoutes (routes storage)
    liftIO $ putDoc storage "avain" "docsi asdasdoasdasda"
    return $ KVBackend storage


toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks
