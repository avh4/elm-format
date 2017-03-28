module Main where

import Cheapskate
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlToByteStringIO)
import Text.Blaze.Html
import System.Environment
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.Text.IO as T
import qualified Data.Text as T
import System.Console.GetOpt
import System.IO (stderr, hPutStr)
import System.Exit
import Data.Version (showVersion)
import Paths_cheapskate (version)
import Control.Monad

convert :: [Option] -> Text -> IO ()
convert opts t = renderHtmlToByteStringIO B.putStr $ toHtml $
  markdown def{ sanitize = Sanitize `elem` opts
              , allowRawHtml = EscapeRawHtml `notElem` opts
              , preserveHardBreaks = HardBreaks `elem` opts
              , debug = Debug `elem` opts
              } t

main :: IO ()
main = do
  argv <- getArgs
  let (flags, args, errs) = getOpt Permute options argv
  let header = "Usage: cheapskate [OPTION..] [FILE..]"
  unless (null errs) $ do
    hPutStr stderr $ usageInfo (unlines $ errs ++ [header]) options
    exitWith $ ExitFailure 1
  when (Version `elem` flags) $ do
    putStrLn $ "cheapskate " ++ showVersion version
    exitWith ExitSuccess
  when (Help `elem` flags) $ do
    putStrLn $ usageInfo header options
    exitWith ExitSuccess
  let handle = convert flags
  case args of
       [] -> T.getContents >>= handle
       _  -> mapM T.readFile args >>= handle . T.unlines

data Option =
    Help | Version | Debug | Sanitize | EscapeRawHtml | HardBreaks
  deriving (Ord, Eq, Show)

options :: [OptDescr Option]
options =
  [ Option ['h'] ["help"] (NoArg Help) "show usage information"
  , Option ['V'] ["version"] (NoArg Version) "show program version"
  , Option [] ["debug"] (NoArg Debug) "print container structure"
  , Option ['s'] ["sanitize"] (NoArg Sanitize) "sanitize output"
  , Option ['e'] ["escape-raw-html"] (NoArg EscapeRawHtml) "escape raw HTML"
  , Option ['b'] ["hard-line-breaks"] (NoArg HardBreaks) "treat newlines as hard breaks"
  ]
