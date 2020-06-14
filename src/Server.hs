{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Server (entryPoint) where

import Data.Aeson (Value)
import Data.FileEmbed (embedDir)

import System.IO.Error (catchIOError)
import System.Exit (exitFailure)
import Text.Printf
import Control.Monad.IO.Class (liftIO)

import Servant
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

import qualified Network.Socket as S
import qualified Network.Socket.ByteString.Lazy as S

import Polysemy hiding (run)
import Polysemy.IO
import Polysemy.Trace

import Config
import Types
import SocketEff
import IPCEff

type CommandAPI
    =    "play_pause" :> Get '[JSON] Value
    :<|> "next"       :> Get '[JSON] Value
    :<|> "prev"       :> Get '[JSON] Value
    :<|> "scrub_back" :> Get '[JSON] Value
    :<|> "scrub_forward" :> Get '[JSON] Value
    :<|> "subtitles"  :> Get '[JSON] Value

apiServer :: Members '[IPC] r => ServerT CommandAPI (Sem r)
apiServer = play_pause :<|> next :<|> prev
          :<|> scrub_back :<|> scrub_forward :<|> subtitles
    where
        sendCommand = sendMessage . Command
        play_pause = sendCommand ["cycle","pause"]
        next       = sendCommand ["playlist-next"]
        prev       = sendCommand ["playlist-prev"]
        scrub_back = sendCommand ["seek","-15","relative"]
        scrub_forward = sendCommand ["seek","15","relative"]
        subtitles  = sendCommand ["cycle","sid"]

type StaticAPI = Raw

staticServer :: ServerT StaticAPI m
staticServer = serveDirectoryEmbedded $(embedDir "static/")

type API
    = "api" :> CommandAPI
    :<|> StaticAPI

server :: Members '[IPC] r => ServerT API (Sem r)
server = apiServer :<|> staticServer

-- natural transformation, tells the server how to run polysemy effects
nt :: Config -> S.Socket -> Sem '[IPC, Socket, Trace, Embed IO] a -> Handler a
nt config sock
    = liftIO
    . runM
    . (if (verbose config) then traceToIO else ignoreTrace)
    . runSocket config sock
    . runIPC

-- create a Warp Application from our API server
app :: Config -> S.Socket -> Application
app config sock = serve api $ hoistServer api (nt config sock) server
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
    let config = defaultConfig
    sock <- catchIOError (openSocket (socketPath config)) $ \e -> do
        putStrLn "Could not connect to IPC socket, please make sure that mpv is running with the --input-ipc-server option"
        exitFailure
    putStrLn $ printf "Listening on port %d." (port config)
    run (port config) (app config sock)
