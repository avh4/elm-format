{-# OPTIONS_GHC -Wall #-}
module Box where

import Elm.Utils ((|>))
import Text.PrettyPrint.Boxes ((<>), (//))

import qualified Data.List as List
import qualified Text.PrettyPrint.Boxes as B


data Box = Box
    { box :: B.Box
    , bottomMargin :: Int
    , hasSize :: Bool
    }


empty :: Box
empty =
    Box B.nullBox 0 False


hspace :: Int -> Box
hspace c =
    Box (B.emptyBox 0 c) 0 (c > 0)


vspace :: Int -> Box
vspace r =
    Box (B.emptyBox r 0) 0 (r > 0)


text :: String -> Box
text s =
    Box (B.text s) 0 (s /= "")


vbox2 :: Box -> Box -> Box
vbox2 (Box a aMargin ae) (Box b bMargin be) =
    Box (a // B.emptyBox aMargin 0 // b) bMargin (ae || be)


vbox :: [Box] -> Box
vbox =
    foldl vbox2 empty


vboxlist :: Box -> String -> Box -> (a -> Box) -> [a] -> Box
vboxlist start mid end format items =
    case items of
        [] ->
            hbox2 start end
        (first:rest) ->
            vbox $
                [ hbox2margin start (format first) ]
                ++ (List.map (hbox2margin (text mid) . format) rest) ++
                if hasSize end then [ end ] else []


hbox2 :: Box -> Box -> Box
hbox2 (Box a _ ae) (Box b _ be) =
    Box (a <> b) 0 (ae || be)


hbox2margin :: Box -> Box -> Box
hbox2margin (Box a aMargin ae) (Box b bMargin be) =
    Box (a <> b) (max aMargin bMargin) (ae || be)


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


indent :: Int -> Box -> Box
indent i child =
    hbox2margin (hspace i) child


margin :: Int -> Box -> Box
margin m (Box child _ ae) =
    Box child m ae


render :: Box -> String
render (Box child _ _) =
    B.render child
