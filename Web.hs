{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString,fromChunks)
import Data.Conduit
import Data.Conduit.List (consume)
import Network.HTTP.Types (ok200,badRequest400)
import Network.Wai
import Network.Wai.Handler.Warp (run)

import KeyValueStorage

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    var <- initStorage
    run port $ app var

app :: Storage -> Application
app var req = case (requestMethod req,pathInfo req) of
  ("GET",["doc",key,"wait"]) -> do
    doc <- liftIO $ waitDoc var key
    binaryResponse doc
  ("GET",["doc",key]) -> do
    mbDoc <- liftIO $ getDoc var key
    case mbDoc of
      Just doc -> binaryResponse doc
      Nothing -> bad "Document not found"
  ("DELETE",["doc",key]) -> do
    deleted <- liftIO $ delDoc var key
    if deleted 
      then good "Deleted"
      else bad "Not found"
  ("PUT",["doc",key]) -> do
      value <- requestBody req $$ sinkLbs
      overwritten <- liftIO $ putDoc var key value
      good $ if overwritten then "Overwritten" else "Created"
  ("GET",["dir"]) -> do
    values <- liftIO $ getDocs var
    good values
  _ -> bad "Unknown command"

bad,good :: Monad m => ByteString -> m Response
bad  = textualResponse badRequest400 
good = textualResponse ok200

textualResponse code text = return $
                            responseLBS code
                            [("Content-Type", "text/plain")]
                            text

binaryResponse x = return $
                   responseLBS ok200
                   [("Content-Type", "application/octet-stream")]
                   x

-- | Backported from conduit-1.0.5 module Data.Conduit.Binary
sinkLbs = fmap fromChunks consume
