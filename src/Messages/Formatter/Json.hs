module Messages.Formatter.Json (format, init, done) where

import qualified ElmFormat.Version
import qualified Text.JSON as Json

import Prelude hiding (init, putStr, putStrLn)
import Control.Monad.State
import Messages.Formatter.Format
import Messages.Types
import ElmFormat.World


format :: World m => Bool -> InfoFormatterF a -> StateT Bool m a
format autoYes infoFormatter =
    case infoFormatter of
        OnInfo info next ->
            showInfo info next

        Approve _prompt next ->
            case autoYes of
                True -> return (next True)
                False -> return (next False)


init :: World m => (m (), Bool)
init =
    (putStr "[", False)


done :: World m => m ()
done =
    putStrLn "]"


showInfo :: World m => InfoMessage -> a -> StateT Bool m a

showInfo (ProcessingFile _) next =
    return next

showInfo (FileWouldChange file fileVersion) next =
    json next file $
        "File is not formatted with elm-format-" ++ ElmFormat.Version.asString
        ++ " --elm-version=" ++ show fileVersion

showInfo (ParseError inputFile _ _) next =
    json next inputFile "Error parsing the file"


json :: World m => a -> FilePath -> String -> StateT Bool m a
json next file message =
    do
        printComma <- get
        when printComma (lift $ putStr ",")
        lift $ putStrLn $ Json.encode $ Json.makeObj
            [ ( "path", Json.JSString $ Json.toJSString file )
            , ( "message", Json.JSString $ Json.toJSString message )
            ]
        put True
        return next
