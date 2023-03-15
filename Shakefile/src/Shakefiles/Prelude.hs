module Shakefiles.Prelude where

import Development.Shake


nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n-1) f


hashNeed :: [FilePath] -> Action String
hashNeed files = do
    need files
    liftIO $ getHashedShakeVersion files
