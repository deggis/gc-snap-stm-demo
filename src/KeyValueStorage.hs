{-# LANGUAGE OverloadedStrings #-}
module KeyValueStorage where

import Data.Functor
import Control.Concurrent.STM
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (intersperse)

type Storage = TVar (Map Text ByteString)

-- | Initializes key-value storage
initStorage :: IO Storage
initStorage = newTVarIO M.empty

-- | Retrieves a document from the storage. If no document is found,
-- wait until it is created.
waitDoc :: Storage -> Text -> IO ByteString
waitDoc var key = atomically $ do
  m <- readTVar var
  case M.lookup key m of
    Nothing -> retry
    Just x  -> return x

-- | Retrieves a document from the storage. If no document is found,
-- return Nothing.
getDoc :: Storage -> Text -> IO (Maybe ByteString)
getDoc var key = M.lookup key <$> readTVarIO var

-- | Stores a document. If older document exists, it is
-- overwritten. In case of overwrite, True is returned.
putDoc :: Storage -> Text -> ByteString -> IO Bool
putDoc var key value = atomically $ do
  m <- readTVar var
  writeTVar var $ M.insert key value m
  return $ M.member key m

-- | Deletes a document. If no such document exists, return False.
delDoc :: Storage -> Text -> IO Bool
delDoc var key = atomically $ do
  m <- readTVar var
  if M.member key m
    then do writeTVar var $ M.delete key m 
            return True
    else return False

getDocs :: Storage -> IO ByteString
getDocs var = do
  m <- readTVarIO var
  return $ B.fromChunks $ intersperse "\n" $ map encodeUtf8 $ M.keys m