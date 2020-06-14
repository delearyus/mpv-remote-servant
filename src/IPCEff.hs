{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module IPCEff where

import Data.Aeson (Value(..), encode, decode)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.HashMap.Strict (member)
import Data.Maybe (catMaybes)

import Polysemy
import SocketEff
import Types

data IPC m a where
    SendMessage :: Command -> IPC m Value

makeSem ''IPC

runIPC :: Members '[Socket] r => Sem (IPC ': r) a -> Sem r a
runIPC = interpret $ \case
    SendMessage msg -> do
        writeSocket $ encode msg
        res <- readSocket
        let responses = decode <$> C.split '\n' res :: [Maybe Value]
        let response = head $ filter isResponse $ catMaybes responses
        return response

isResponse :: Value -> Bool
isResponse (Object o) = member "data" o && member "error" o
isResponse _ = False
