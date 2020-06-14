{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Server (entryPoint) where

import Data.Aeson (ToJSON, Value(..), encode, decode)

import System.IO.Error (catchIOError)
import System.Exit (exitFailure)

import Control.Monad.IO.Class (liftIO)

import Servant
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

import qualified Network.Socket as S
import qualified Network.Socket.ByteString.Lazy as S

import Polysemy hiding (run)
import Polysemy.IO

import Config
import Types
import SocketEff
import IPCEff
import API

-- natural transformation, tells the server how to run polysemy effects
nt :: S.Socket -> Sem '[IPC, Socket, Embed IO] a -> Handler a
nt sock = liftIO . runM . runSocket sock . runIPC

-- create a Warp Application from our API server
app :: S.Socket -> Application
app sock = serve api $ hoistServer api (nt sock) server
    where api = Proxy @API

openSocket :: FilePath -> IO S.Socket
openSocket path = do
    sock <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
    S.connect sock $ S.SockAddrUnix path
    return sock

-- Entry Point
---------------------------------------

entryPoint :: IO ()
entryPoint = do
    let Config{..} = defaultConfig
    sock <- catchIOError (openSocket socketPath) $ \e -> do
        putStrLn "Could not connect to IPC socket, please make sure that mpv is running with the --input-ipc-server option"
        exitFailure
    run port (app sock)
