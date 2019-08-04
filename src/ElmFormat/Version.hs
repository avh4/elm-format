module ElmFormat.Version
  ( asString
  , experimental
  )
where

import qualified Build_elm_format


asString :: String
asString = Build_elm_format.gitDescribe


experimental :: Maybe String
experimental = Nothing
