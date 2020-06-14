{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module SocketEff where

import Polysemy
import Polysemy.Trace

import qualified Data.ByteString.Lazy as BS
import Text.Printf (printf)

import qualified Network.Socket as S
import qualified Network.Socket.ByteString.Lazy as S

import Control.Concurrent (threadDelay)

import Config

data Socket m a where
    WriteSocket :: BS.ByteString -> Socket m ()
    ReadSocket  :: Socket m BS.ByteString

makeSem ''Socket

runSocket :: Members [Trace,Embed IO] r
    => Config
    -> S.Socket
    -> Sem (Socket ': r) a
    -> Sem r a
runSocket Config{..} sock = interpret $ \case
    WriteSocket msg -> do
        trace $ printf "[%s] >> %s" socketPath (show msg)
        embed $ S.sendAll sock (msg `BS.append` "\n")
    ReadSocket -> do
        embed $ threadDelay (1000*10) 
        -- TODO this is a race condition but actually fixing it is tricky
        msg <- embed $ S.recv sock 1024
        trace $ printf "[%s] << %s" socketPath (show msg)
        return msg
