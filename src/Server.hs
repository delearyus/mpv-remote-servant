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

module Server (entryPoint) where

import Servant
import Data.Aeson (ToJSON, Value(..), encode, decode)
import Data.Maybe (fromJust, catMaybes)
import Data.HashMap.Strict (member)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types
import qualified Network.Socket as S
import qualified Network.Socket.ByteString.Lazy as S
import Polysemy  (embed, Members, Sem, Member, Embed, interpret, runM, makeSem)
import Polysemy.IO
import Polysemy.Reader

-- CONFIGURATION
-- (replace with a real Reader Config setup later)
---------------------------------------------------

data Config = Config
    { socket :: S.Socket
    , port :: Int
    }

defaultConfig :: IO Config
defaultConfig = do
    sock <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
    S.connect sock (S.SockAddrUnix defaultSocketPath)
    return $ Config { socket = sock, port = defaultPort }

defaultSocketPath :: String
defaultSocketPath = "/tmp/sock"

defaultPort :: Int
defaultPort = 8383

-- DATA TYPES
---------------------------------------------------

data Command = Command { command :: [String] } deriving (Eq,Show,Generic)
instance ToJSON Command

-- EFFECT TYPES
-- (several layers of polysemy effects to abstract actions)
---------------------------------------------------

data IPC m a where
    SendMessage :: Command -> IPC m Value

data Socket m a where
    WriteSocket :: S.Socket -> BS.ByteString -> Socket m ()
    ReadSocket  :: S.Socket -> Socket m BS.ByteString

makeSem ''IPC
makeSem ''Socket

runIPC :: Members '[Reader Config, Socket, Embed IO] r => Sem (IPC ': r) a -> Sem r a
runIPC = interpret $ \case
    SendMessage msg -> do
        config <- ask
        writeSocket (socket config) $ encode msg
        embed $ threadDelay (1000*10) -- TODO this is a race condition but 
                                      -- actually fixing it is tricky
        res <- readSocket (socket config)
        let responses = decode <$> C.split '\n' res :: [Maybe Value]
        let response = head $ filter isResponse $ catMaybes responses
        return response

isResponse :: Value -> Bool
isResponse (Object o) = member "data" o && member "error" o
isResponse _ = False

runSocket :: Member (Embed IO) r => Sem (Socket ': r) a -> Sem r a
runSocket = interpret $ \case
    WriteSocket sock msg -> do
        embed $ BS.putStr msg
        embed $ putStrLn ""
        embed $ S.sendAll sock (msg `BS.append` "\n")
    ReadSocket sock -> do
        embed $ putStrLn $ "reading"
        res <- embed $ S.recv sock 1024
        return res

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
staticServer = serveDirectoryWebApp "static/"

type API
    = "api" :> CommandAPI
    :<|> StaticAPI

server :: Members '[IPC] r => ServerT API (Sem r)
server = apiServer :<|> staticServer


-- Entry Point
---------------------------------------

nt :: Config -> Sem '[IPC, Socket, Reader Config, Embed IO] a -> Handler a
nt config = liftIO . runM . runReader config . runSocket . runIPC

app :: Config -> Application
app config = serve (Proxy @API) $ hoistServer (Proxy @API) (nt config) server

entryPoint :: IO ()
entryPoint = do
    config <- defaultConfig
    run (port config) (app config)
