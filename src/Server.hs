{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Server (entryPoint) where

import Data.Aeson (ToJSON, Value(..), encode, decode)
import GHC.Generics (Generic)

import System.IO.Error (catchIOError)
import System.Exit (exitFailure)

import Data.Maybe (fromJust, catMaybes)
import Data.HashMap.Strict (member)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C
import Data.FileEmbed (embedDir)

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

import Servant
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

import qualified Network.Socket as S
import qualified Network.Socket.ByteString.Lazy as S

import Polysemy hiding (run)
import Polysemy.IO
import Polysemy.Reader

-- CONFIGURATION
---------------------------------------------------

data Config = Config
    { socketPath :: FilePath
    , port :: Int
    }

defaultConfig :: Config
defaultConfig = Config
    { socketPath = "/tmp/mpvsocket"
    , port = 8383
    }

-- DATA TYPES
---------------------------------------------------

data Command = Command { command :: [String] } deriving (Eq,Show,Generic)
instance ToJSON Command

-- EFFECT TYPES
-- (several layers of polysemy effects to provide layers of abstraction)
---------------------------------------------------

data IPC m a where
    SendMessage :: Command -> IPC m Value

data Socket m a where
    WriteSocket :: BS.ByteString -> Socket m ()
    ReadSocket  :: Socket m BS.ByteString

makeSem ''IPC
makeSem ''Socket

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

-- SERVANT API
---------------------------------------------------

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


-- Helper Functions
---------------------------------------

-- natural transformation, tells the server how to run polysemy effects
nt :: S.Socket -> Sem '[IPC, Socket, Embed IO] a -> Handler a
nt sock = liftIO . runM . runSocket sock . runIPC

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
