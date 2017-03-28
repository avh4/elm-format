{-# LANGUAGE OverloadedStrings #-}
module Main where
import Network.Wai.Handler.CGI
import Network.Wai
import Control.Applicative ((<$>))
import Data.Maybe (mapMaybe, fromMaybe)
import Network.HTTP.Types.Status (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.URI (queryToQueryText)
import Cheapskate
import Paths_cheapskate (version)
import Data.Version (showVersion)
import qualified Data.Text as T
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html
import Data.Aeson
import qualified Data.Text as T
import Data.Text (Text)
import System.Timeout

main :: IO ()
main = do
  mbres <- timeout (4 * 10^6) $ run app
  case mbres of
        Just r  -> return r
        Nothing -> error "Pandoc timed out."

app :: Application
app req respond = do
  let query = queryToQueryText $ queryString req
  let getParam x = maybe (error $ T.unpack x ++ " parameter not set")
                       return $ lookup x query
  text <- getParam "text" >>= checkLength . fromMaybe T.empty
  let html = renderHtml $ toHtml $ markdown def{ sanitize = False } text
  let output = encode $ object [ T.pack "name" .= T.pack "cheapskate"
                                , T.pack "html" .= html
                                , T.pack "version" .= T.pack (showVersion version)]
  respond $ responseLBS status200 [(hContentType,"text/json; charset=UTF-8")] output

checkLength :: Text -> IO Text
checkLength t =
  if T.length t > 1000
     then error "exceeds length limit of 1000 characters"
     else return t

