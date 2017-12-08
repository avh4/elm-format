module Messages.Formatter.Json (format, init, done) where

import qualified ElmFormat.Version
import qualified Text.JSON as Json

import Prelude hiding (init, putStr, putStrLn)
import Control.Monad.State
import Messages.Formatter.Format
import Messages.Types
import ElmVersion (ElmVersion)
import ElmFormat.World


format :: World m => ElmVersion -> Bool -> InfoFormatterF a -> StateT Bool m a
format elmVersion autoYes infoFormatter =
    case infoFormatter of
        OnInfo info next ->
            showInfo elmVersion info next

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


showInfo :: World m => ElmVersion -> InfoMessage -> a -> StateT Bool m a

showInfo _ (ProcessingFile _) next =
    return next

showInfo elmVersion (FileWouldChange file) next =
    json next file $
        "File is not formatted with elm-format-" ++ ElmFormat.Version.asString
        ++ " --elm-version=" ++ show elmVersion

showInfo _ (ParseError inputFile _ _) next =
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
