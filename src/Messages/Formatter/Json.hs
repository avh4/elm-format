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
    Just $ Json.encode $ Json.makeObj
        [ ( "path", Json.JSString $ Json.toJSString file )
        , ( "message"
          , Json.JSString $ Json.toJSString
              $ "File is not formatted with elm-format-" ++ ElmFormat.Version.asString
                  ++ " --elm-version=" ++ show elmVersion
          )
        ]
