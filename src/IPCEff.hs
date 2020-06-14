{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module IPCEff where

import Data.Aeson (Value(..), encode, decode)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.HashMap.Strict (member)
import Data.Maybe (catMaybes)
import Text.Printf

import Polysemy
import Polysemy.Trace
import SocketEff
import Types


data IPC m a where
    SendMessage :: Command -> IPC m Value

makeSem ''IPC

runIPC :: Members '[Socket, Trace] r => Sem (IPC ': r) a -> Sem r a
runIPC = interpret $ \case
    SendMessage msg -> do
        trace $ printf "[request]: %s" $ show msg
        writeSocket $ encode msg
        res <- readSocket
        let responses = decode <$> C.split '\n' res :: [Maybe Value]
        let response = head $ filter isResponse $ catMaybes responses
        trace $ printf "[response]: %s" $ C.unpack $ encode response
        return response

isResponse :: Value -> Bool
isResponse (Object o) = member "data" o && member "error" o
isResponse _ = False
