{-# OPTIONS_GHC -Wall #-}
module Box where

import Elm.Utils ((|>))
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

vboxlist :: String -> String -> String -> (a -> Box) -> [a] -> Box
vboxlist start mid end format items =
    case items of
        [] ->
            hbox2 (text start) (text end)
        (first:rest) ->
            vbox $
                [ hbox2 (text start) (format first) ]
                ++ (List.map (hbox2 (text mid) . format) rest) ++
                [ text end ]


hbox2 :: Box -> Box -> Box
hbox2 (Box a aMargin) (Box b bMargin) =
    Box (a <> b) (max aMargin bMargin)


hbox :: [Box] -> Box
hbox =
    foldl hbox2 empty


hboxlist :: String -> String -> String -> (a -> Box) -> [a] -> Box
hboxlist start mid end format items =
    hbox $
        [ text start ]
        ++ (List.map format items |> List.intersperse (text mid)) ++
        [ text end ]


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
