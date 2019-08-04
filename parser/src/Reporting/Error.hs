{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error where

import           Data.Aeson                               ( (.=) )
import qualified Data.Aeson                    as Json
import           Prelude                           hiding ( print )

import qualified Reporting.Annotation          as A
import qualified Reporting.Error.Docs          as Docs
import qualified Reporting.Error.Syntax        as Syntax
import qualified Reporting.Report              as Report


-- ALL POSSIBLE ERRORS

data Error
    = Syntax Syntax.Error
    | Docs Docs.Error
    deriving (Eq, Show)


-- TO REPORT

toReport :: Error -> Report.Report
toReport err = case err of
  Syntax syntaxError -> Syntax.toReport syntaxError

  Docs   docsError   -> Docs.toReport docsError


-- TO STRING

toString :: String -> String -> A.Located Error -> String
toString location source (A.A region err) =
  Report.toString location region (toReport err) source


print :: String -> String -> A.Located Error -> IO ()
print location source (A.A region err) =
  Report.printError location region (toReport err) source


-- TO JSON

toJson :: FilePath -> A.Located Error -> Json.Value
toJson filePath (A.A region err) =
  let (maybeRegion, additionalFields) = case err of
        Syntax syntaxError -> Report.toJson [] (Syntax.toReport syntaxError)

        Docs   docsError   -> Report.toJson [] (Docs.toReport docsError)
  in  Json.object
        $  [ "file" .= filePath
           , "region" .= region
           , "subregion" .= maybeRegion
           , "type" .= ("error" :: String)
           ]
        ++ additionalFields
