module Messages.Formatter.Json (format) where

import qualified ElmFormat.Version
import qualified Text.JSON as Json

import Messages.Formatter.Format
import Messages.Types
import ElmVersion (ElmVersion)


format :: ElmVersion -> InfoFormatter
format elmVersion = InfoFormatter
    { onInfo = maybe (return ()) putStrLn . showInfo elmVersion }


showInfo :: ElmVersion -> InfoMessage -> Maybe String

showInfo _ (ProcessingFiles _) =
    Nothing

showInfo elmVersion (FileWouldChange file) =
    Just $ json file $
        "File is not formatted with elm-format-" ++ ElmFormat.Version.asString
        ++ " --elm-version=" ++ show elmVersion

showInfo _ (ParseError inputFile _ _) =
    Just $ json inputFile "Error parsing the file"


json :: FilePath -> String -> String
json file message =
    Json.encode $ Json.makeObj
        [ ( "path", Json.JSString $ Json.toJSString file )
        , ( "message", Json.JSString $ Json.toJSString message )
        ]
