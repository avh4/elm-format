module ElmFormat.Version (asString) where

import qualified Paths_elm_format as This
import qualified Build_elm_format

import Data.Version (showVersion)


asString :: String
asString =
    Build_elm_format.gitDescribe
