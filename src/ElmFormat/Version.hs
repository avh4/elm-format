module ElmFormat.Version (asString) where

import qualified Build_elm_format


asString :: String
asString =
    Build_elm_format.gitDescribe
