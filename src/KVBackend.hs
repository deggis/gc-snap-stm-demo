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
import Control.Monad
import Control.Monad.State

import Data.Text.Encoding
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)

import KeyValueStorage

data KVBackend = KVBackend { storage :: Storage }

2type KVHandler r = forall a. Handler a KVBackend r

  
getWaitDocR :: KVHandler ()
getWaitDocR = withStorageAndKey $ \(storage,key) -> do
  doc <- liftIO $ waitDoc storage k
  writeBS . toStrict $ doc

getDocR :: KVHandler ()
getDocR = withStorageAndKey $ \(storage,key) -> do
  mbDoc <- liftIO $ getDoc storage k
  case mbDoc of
      Just doc -> writeBS . toStrict $ doc
      _        -> bad "Not found"

deleteDocR :: KVHandler ()
deleteDocR = withStorageAndKey $ \(storage,key) -> do
  deleted <- liftIO $ delDoc storage key
  if deleted 
   then good "Deleted"
   else bad "Not found"

putDocR :: KVHandler ()
putDocR = withStorageAndKey $ \(storage,key) -> do
  value <- getRequestBody
  overwritten <- liftIO $ putDoc storage key value
  good $ if overwritten then "Overwritten" else "Created"

getDirR :: KVHandler ()
getDirR = withStorageAndKey $ \(storage,key) -> do
  values <- liftIO $ getDocs storage
  good values

routes = [("/hello",         writeText "hello world with snap")
         ,("/doc/:key",      method GET    $ getDocR)
         ,("/doc/:key/wait", method GET    $ getWaitDocR)
         ,("/doc/:key",      method DELETE $ deleteDocR)
         ,("/doc/:key",      method PUT    $ putDocR)
         ,("/dir",           method GET    $ getDirR)
         ]

initKVBackend :: SnapletInit a KVBackend
initKVBackend = makeSnaplet "KVBackend" "STM dojo web with Snaplet" Nothing $ do
    storage <- liftIO initStorage
    addRoutes routes
    liftIO $ putDoc storage "avain" "docsi asdasdoasdasda"
    return $ KVBackend storage


-- helpers

bad,good :: ByteString -> KVHandler ()
bad msg  = modifyResponse (setResponseStatus 400 "Bad request") >> writeBS msg
good msg = writeBS msg

withStorage :: (Storage -> KVHandler ()) -> KVHandler ()
withStorage handler = storage <$> get >>= \s -> handler s

withKey :: (Text -> KVHandler()) -> KVHandler ()
withKey handler = do
  mbKey <- fmap decodeUtf8 <$> getParam "key"
  case mbKey of
      Just key  -> handler key
      Nothing   -> bad "Key missing"

withStorageAndKey :: ((Storage,Text) -> KVHandler ()) -> KVHandler ()
withStorageAndKey handler =
    withStorage $ \storage ->
      withKey $ \key ->
        handler (storage,key)


toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks
