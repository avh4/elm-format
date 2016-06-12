module ElmFormat.World where

class Monad m => World m where
    readFile :: FilePath -> m String
    writeFile :: FilePath -> String -> m ()

-- instance World IO where
