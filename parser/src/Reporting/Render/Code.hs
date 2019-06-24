{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Render.Code
  ( Source
  , toSource
  , render
  , CodePair(..)
  , renderPair
  )
  where


import qualified Data.List as List
import qualified Data.Text as Text
import Data.Word (Word16)

import Reporting.Doc (Doc, (<>))
import qualified Reporting.Doc as D
import qualified Reporting.Annotation as A



-- CODE


newtype Source =
  Source [(Int, Text.Text)]


toSource :: Text.Text -> Source
toSource source =
  Source $ zip [1..] $
    Text.lines source ++ [Text.empty]



-- RENDER


(|>) :: a -> (a -> b) -> b
(|>) a f =
  f a


render :: Source -> A.Region -> Maybe A.Region -> Doc
render (Source sourceLines) region@(A.Region (A.Position startLine _) (A.Position endLine _)) maybeSubRegion =
  let
    relevantLines =
      sourceLines
        |> drop (fromIntegral (startLine - 1))
        |> take (fromIntegral (1 + endLine - startLine))

    width =
      length (show (fst (last relevantLines)))

    smallerRegion =
      maybe region id maybeSubRegion
  in
    case makeUnderline width endLine smallerRegion of
      Nothing ->
        drawLines True width smallerRegion relevantLines D.empty

      Just underline ->
        drawLines False width smallerRegion relevantLines underline

makeUnderline :: Int -> Word16 -> A.Region -> Maybe Doc
makeUnderline width realEndLine (A.Region (A.Position start c1) (A.Position end c2)) =
  if start /= end || end < realEndLine then
    Nothing

  else
    let
      spaces = replicate (fromIntegral c1 + width + 1) ' '
      zigzag = replicate (max 1 (fromIntegral (c2 - c1))) '^'
    in
      Just (D.fromChars spaces <> D.dullred (D.fromChars zigzag))


drawLines :: Bool -> Int -> A.Region -> [(Word16, String)] -> Doc -> Doc
drawLines addZigZag width (A.Region (A.Position startLine _) (A.Position endLine _)) sourceLines finalLine =
  D.vcat $
    map (drawLine addZigZag width startLine endLine) sourceLines
    ++ [finalLine]


drawLine :: Bool -> Int -> Word16 -> Word16 -> (Word16, String) -> Doc
drawLine addZigZag width startLine endLine (n, line) =
  addLineNumber addZigZag width startLine endLine n (D.fromChars line)


addLineNumber :: Bool -> Int -> Word16 -> Word16 -> Word16 -> Doc -> Doc
addLineNumber addZigZag width start end n line =
  let
    number =
      show n

    lineNumber =
      replicate (width - length number) ' ' ++ number ++ "|"

    spacer =
      if addZigZag && start <= n && n <= end then
        D.dullred ">"
      else
        " "
  in
    D.fromChars lineNumber <> spacer <> line



-- RENDER PAIR


data CodePair
  = OneLine Doc
  | TwoChunks Doc Doc


renderPair :: Source -> A.Region -> A.Region -> CodePair
renderPair source@(Source sourceLines) region1 region2 =
  let
    (A.Region (A.Position startRow1 startCol1) (A.Position endRow1 endCol1)) = region1
    (A.Region (A.Position startRow2 startCol2) (A.Position endRow2 endCol2)) = region2
  in
  if startRow1 == endRow1 && endRow1 == startRow2 && startRow2 == endRow2 then
    let
      lineNumber = show startRow1
      spaces1 = replicate (fromIntegral startCol1 + length lineNumber + 1) ' '
      zigzag1 = replicate (fromIntegral (endCol1 - startCol1)) '^'
      spaces2 = replicate (fromIntegral (startCol2 - endCol1)) ' '
      zigzag2 = replicate (fromIntegral (endCol2 - startCol2)) '^'

      (Just line) = List.lookup startRow1 sourceLines
    in
    OneLine $
      D.vcat
        [ D.fromChars lineNumber <> "| " <> D.fromChars line
        , D.fromChars spaces1 <> D.dullred (D.fromChars zigzag1) <>
          D.fromChars spaces2 <> D.dullred (D.fromChars zigzag2)
        ]

  else
    TwoChunks
      (render source region1 Nothing)
      (render source region2 Nothing)
