module Messages.Formatter.Json (format) where

import qualified ElmFormat.Version
import qualified Text.JSON as Json

import Messages.Formatter.Format
import Messages.Types


format :: InfoFormatter
format = InfoFormatter
    { onInfo = maybe (return ()) putStrLn . showInfo }


showInfo :: InfoMessage -> Maybe String

showInfo (ProcessingFiles _) =
    Nothing

showInfo (FileWouldChange file) =
    Just $ Json.encode $ Json.makeObj
        [ ( "path", Json.JSString $ Json.toJSString file )
        , ( "message"
          , Json.JSString $ Json.toJSString
              $ "File is not formatted with elm-format-" ++ ElmFormat.Version.asString
          )
        ]
