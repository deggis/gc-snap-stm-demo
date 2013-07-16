{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module KVBackend where

import Snap.Snaplet
import Snap.Core

import KeyValueStorage

data KVBackend = KVBackend String

initKVBackend :: SnapletInit a KVBackend
initKVBackend = makeSnaplet "KVBackend" "STM dojo web with Snaplet" Nothing $ do
    addRoutes [ ("/hello", writeText "hello world with snap") ]
    return $ KVBackend "init"


