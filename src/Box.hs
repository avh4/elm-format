{-# OPTIONS_GHC -Wall #-}
module Box where

import Text.PrettyPrint.Boxes ((<>), (//))

import qualified Data.List as List
import qualified Text.PrettyPrint.Boxes as B


data Box = Box
    { box :: B.Box
    , bottomMargin :: Int
    }


empty :: Box
empty =
    Box B.nullBox 0


hspace :: Int -> Box
hspace c =
    Box (B.emptyBox 0 c) 0


vspace :: Int -> Box
vspace r =
    Box (B.emptyBox r 0) 0


text :: String -> Box
text s =
    Box (B.text s) 0


vbox2 :: Box -> Box -> Box
vbox2 (Box a aMargin) (Box b bMargin) =
    Box (a // B.emptyBox aMargin 0 // b) bMargin


vbox :: [Box] -> Box
vbox =
    foldl vbox2 empty


hbox2 :: Box -> Box -> Box
hbox2 (Box a aMargin) (Box b bMargin) =
    Box (a <> b) (max aMargin bMargin)


hbox :: [Box] -> Box
hbox =
    foldl hbox2 empty


hjoin :: Box -> [Box] -> Box
hjoin sep list =
    hbox (List.intersperse sep list)


indent :: Box -> Box
indent child =
    hbox2 (hspace 4) child


margin :: Int -> Box -> Box
margin m (Box child _) =
    Box child m


render :: Box -> String
render (Box child _) =
    B.render child
