module Shakefiles.Extra (phonyPrefix, forEach) where

import Development.Shake
import Data.List (stripPrefix)


phonyPrefix :: String -> (String -> Action ()) -> Rules ()
phonyPrefix prefix action =
    phonys $ \s -> case stripPrefix prefix s of
        Nothing -> Nothing
        Just match -> Just (action match)



forEach :: Monad m => [a] -> (a -> m ()) -> m ()
forEach list f =
    mapM_ f list
