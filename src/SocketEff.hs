{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module SocketEff where

import Polysemy
import qualified Data.ByteString.Lazy as BS
import qualified Network.Socket as S
import qualified Network.Socket.ByteString.Lazy as S
import Control.Concurrent (threadDelay)

data Socket m a where
    WriteSocket :: BS.ByteString -> Socket m ()
    ReadSocket  :: Socket m BS.ByteString

makeSem ''Socket

runSocket :: Member (Embed IO) r => S.Socket -> Sem (Socket ': r) a -> Sem r a
runSocket sock = interpret $ \case
    WriteSocket msg -> embed $ do
        BS.putStr msg
        putStrLn ""
        S.sendAll sock (msg `BS.append` "\n")
    ReadSocket -> embed $ do
        putStrLn $ "reading"
        threadDelay (1000*10) -- TODO this is a race condition but 
                              -- actually fixing it is tricky
        S.recv sock 1024
