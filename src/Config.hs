module Config where

data Config = Config
    { socketPath :: FilePath
    , port :: Int
    , verbose :: Bool
    }

defaultConfig :: Config
defaultConfig = Config
    { socketPath = "/tmp/mpvsocket"
    , port = 8383
    , verbose = False
    }
