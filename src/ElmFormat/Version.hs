module ElmFormat.Version (asString) where

import qualified Paths_elm_format as This

import Data.Version (showVersion)


asString :: String
asString =
    showVersion This.version  ++ "-alpha"
