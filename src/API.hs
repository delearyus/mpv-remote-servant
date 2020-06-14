{-# LANGUAGE TemplateHaskell #-}
module API where

import Servant
import Polysemy (Sem, Members)
import Data.Aeson (Value)
import Data.FileEmbed (embedDir)

import IPCEff
import Types

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
