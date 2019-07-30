{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Report
    ( Report(Report)
    , simple
    , toString
    , printError, printWarning
    ) where

import Control.Applicative ((<|>))
import Control.Monad.Writer (Writer, execWriter, tell)
import qualified Data.List.Split as Split
import System.Console.ANSI
import ElmFormat.World

import qualified Reporting.Region as R


data Report = Report
    { _title :: String
    , _highlight :: Maybe R.Region
    , _preHint :: String
    , _postHint :: String
    }
    deriving (Show)


simple :: String -> String -> String -> Report
simple title pre post =
  Report title Nothing pre post


toString :: String -> R.Region -> Report -> String -> String
toString location region report source =
  execWriter (render plain location region report source)


printError :: World m => String -> R.Region -> Report -> String -> m ()
printError location region report source =
  render (ansi Error) location region report source


printWarning :: World m => String -> R.Region -> Report -> String -> m ()
printWarning location region report source =
  render (ansi Warning) location region report source


render
    :: (Monad m)
    => Renderer m
    -> String
    -> R.Region
    -> Report
    -> String
    -> m ()
render renderer location region (Report title highlight pre post) source =
  do  messageBar renderer title location
      normal renderer (pre ++ "\n\n")
      grabRegion renderer highlight region source
      normal renderer ("\n" ++ if null post then "\n" else post ++ "\n\n\n")


-- RENDERING

data Renderer m = Renderer
    { normal :: String -> m ()
    , header :: String -> m ()
    , accent :: String -> m ()
    }


plain :: Renderer (Writer String)
plain =
  Renderer tell tell tell


data Type = Error | Warning


ansi :: World m => Type -> Renderer m
ansi tipe =
  let
    put =
      putStrStderr

    put' intensity color string =
      do  putSgrStderr [SetColor Foreground intensity color]
          put string
          putSgrStderr [Reset]

    accentColor =
      case tipe of
        Error -> Red
        Warning -> Yellow
  in
    Renderer
      put
      (put' Dull Cyan)
      (put' Dull accentColor)




-- REPORT HEADER

messageBar :: Renderer m -> String -> String -> m ()
messageBar renderer tag location =
  let
    usedSpace = 4 + length tag + 1 + length location
  in
    header renderer $
      "-- " ++ tag ++ " "
      ++ replicate (max 1 (80 - usedSpace)) '-'
      ++ " " ++ location ++ "\n\n"


-- REGIONS

grabRegion
    :: (Monad m)
    => Renderer m
    -> Maybe R.Region
    -> R.Region
    -> String
    -> m ()
grabRegion renderer maybeSubRegion region@(R.Region start end) source =
  let
    (R.Position startLine startColumn) = start
    (R.Position endLine endColumn) = end

    (|>) = flip ($)

    relevantLines =
        -- Using `lines` here will strip the last line.
        Split.splitOn "\n" source
          |> drop (startLine - 1)
          |> take (endLine - startLine + 1)
  in
  case relevantLines of
    [] ->
        normal renderer ""

    [sourceLine] ->
        singleLineRegion renderer startLine sourceLine $
          case maybeSubRegion of
            Nothing ->
                (0, startColumn, endColumn, length sourceLine)

            Just (R.Region s e) ->
                (startColumn, R.column s, R.column e, endColumn)

    firstLine : rest ->
        let
          filteredFirstLine =
              replicate (startColumn - 1) ' '
              ++ drop (startColumn - 1) firstLine

          filteredLastLine =
              take (endColumn) (last rest)

          focusedRelevantLines =
              filteredFirstLine : init rest ++ [filteredLastLine]

          lineNumbersWidth =
              length (show endLine)

          subregion =
              maybeSubRegion <|> Just region

          numberedLines =
              zipWith
                (addLineNumber renderer subregion lineNumbersWidth)
                [startLine .. endLine]
                focusedRelevantLines
        in
          mapM_ (\line -> line >> normal renderer "\n") numberedLines


addLineNumber
    :: (Monad m)
    => Renderer m
    -> Maybe R.Region
    -> Int
    -> Int
    -> String
    -> m ()
addLineNumber renderer maybeSubRegion width n line =
  let
    number =
      if n < 0 then " " else show n

    lineNumber =
      replicate (width - length number) ' ' ++ number ++ "|"

    spacer (R.Region start end) =
      if R.line start <= n && n <= R.line end
        then accent renderer ">"
        else normal renderer " "
  in
    do  normal renderer lineNumber
        maybe (normal renderer " ") spacer maybeSubRegion
        normal renderer line


singleLineRegion
    :: (Monad m)
    => Renderer m
    -> Int
    -> String
    -> (Int, Int, Int, Int)
    -> m ()
singleLineRegion renderer lineNum sourceLine (start, innerStart, innerEnd, end) =
  let
    width =
      length (show lineNum)

    underline =
      replicate (innerStart + width + 1) ' '
      ++ replicate (max 1 (innerEnd - innerStart)) '^'

    (|>) = flip ($)

    trimmedSourceLine =
        sourceLine
          |> drop (start - 1)
          |> take (end - start + 1)
          |> (++) (replicate (start - 1) ' ')
  in
    do  addLineNumber renderer Nothing width lineNum trimmedSourceLine
        accent renderer $ "\n" ++ underline
