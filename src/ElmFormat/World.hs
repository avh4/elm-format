module ElmFormat.World where

class Monad m => World m where
    readFile :: FilePath -> m String
    writeFile :: FilePath -> String -> m ()
    putStrLn :: String -> m ()
    getProgName :: m String

-- instance World IO where
