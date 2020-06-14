module Config where

data Config = Config
    { socketPath :: FilePath
    , port :: Int
    }

defaultConfig :: Config
defaultConfig = Config
    { socketPath = "/tmp/mpvsocket"
    , port = 8383
    }
