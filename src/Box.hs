{-# OPTIONS_GHC -Wall #-}
module Box where

import Elm.Utils ((|>))
import Text.PrettyPrint.Boxes ((<>), (//))

import qualified Data.List as List
import qualified Text.PrettyPrint.Boxes as B


data Line
    = Text String
    | Row [Line]
    | Space
    | Tab


identifier :: String -> Line
identifier =
    Text


keyword :: String -> Line
keyword =
    Text


punc :: String -> Line
punc =
    Text


literal :: String -> Line
literal =
    Text


row :: [Line] -> Line
row =
    Row


space :: Line
space =
    Space


data Box
    = Stack Line [Line]
    -- | Margin Int
    -- | Empty


line :: Line -> Box
line l =
    Stack l []


stack' :: Box -> Box -> Box
stack' b1 b2 =
     case (b1, b2) of
        (Stack l11 l1n, Stack l21 l2n) ->
            Stack l11 (l1n ++ (l21:l2n))


stack :: [Box] -> Box
stack children =
    case children of
        -- [] -> -- crash? -- TODO?
        (first:[]) ->
            first
        (first:rest) ->
            foldl1 stack' (first:rest)


mapLines :: (Line -> Line) -> Box -> Box
mapLines fn =
    mapFirstLine fn fn


mapFirstLine :: (Line -> Line) -> (Line -> Line) -> Box -> Box
mapFirstLine firstFn restFn b =
    case b of
        Stack l1 ls ->
            Stack (firstFn l1) (map restFn ls)


indent :: Box -> Box
indent =
    mapLines (\l -> row [Tab, l])

-- boxMap :: (Line -> a) -> (Box -> a) -> Box -> a
-- boxMap singleFn boxFn b =
--     case b of
--         Single l ->
--             singleFn l
--         Stack _ ->
--             boxFn b


-- TODO: should return Either [Box] [Line] -- replace isLine and destructure
-- TODO: use destructure instead?
isLine :: Box -> Maybe Line
isLine b =
    case b of
        Stack l [] ->
            Just l
        _ ->
            Nothing


destructure :: Box -> (Line, [Line])
destructure b =
    case b of
        Stack first rest ->
            (first, rest)


-- TODO: should return Either [Box] [Line]
allSingles :: [Box] -> Maybe [Line]
allSingles =
    sequence . (map isLine)


elmApplication :: Box -> [Box] -> Box
elmApplication first rest =
    case allSingles (first:rest) of
        Just ls ->
            ls
                |> List.intersperse space
                |> row
                |> line
        _ ->
            stack
                $ first : (map indent rest)


prefix :: Line -> Box -> Box
prefix pref =
    mapFirstLine
        (\l -> row [ pref, space, l ])
        (\l -> row [ space, space, l ])


elmGroup :: String -> String -> String -> Bool -> [Box] -> Box
elmGroup left sep right forceMultiline children =
    case (forceMultiline, allSingles children) of
        (_, Just []) ->
            line $ row [punc left, punc right]
        (False, Just ls) ->
            line $ row $ concat
                [ [punc left, space]
                , List.intersperse (row [punc sep, space]) ls
                , [space, punc right]
                ]
        _ ->
            case children of
                [] ->
                    line $ row [ punc left, punc right]
                (first:rest) ->
                    stack $
                        (prefix (punc left) first)
                        : (map (prefix (punc sep)) rest)
                        ++ [ line $ punc right ]


-- DEPRECATED BELOW THIS LINE

data Box' = Box'
    { box :: B.Box
    , bottomMargin :: Int
    , hasSize :: Bool
    }


depr' :: Int -> Line -> (Int, Box')
depr' margin l =
    case l of
        Text s ->
            (0, text s)
        Row items ->
            foldl
                (\(m,b) l -> let (mm,bb) = depr' m l in (mm, hbox2 b bb))
                (margin,empty)
                items
        Space ->
            (margin + 1, text " ")
        Tab ->
            (0, text $ replicate (4 - (margin `mod` 4)) ' ')


depr :: Box -> Box'
depr b =
    case b of
        Stack first rest ->
            vbox (map (snd . depr' 0) (first:rest))


empty :: Box'
empty =
    Box' B.nullBox 0 False


hspace :: Int -> Box'
hspace c =
    Box' (B.emptyBox 0 c) 0 (c > 0)


vspace :: Int -> Box'
vspace r =
    Box' (B.emptyBox r 0) 0 (r > 0)


text :: String -> Box'
text s =
    Box' (B.text s) 0 (s /= "")


vbox2 :: Box' -> Box' -> Box'
vbox2 (Box' a aMargin ae) (Box' b bMargin be) =
    Box' (a // B.emptyBox aMargin 0 // b) bMargin (ae || be)


vbox :: [Box'] -> Box'
vbox =
    foldl vbox2 empty


vboxlist :: Box' -> String -> Box' -> (a -> Box') -> [a] -> Box'
vboxlist start mid end format items =
    case items of
        [] ->
            hbox2 start end
        (first:rest) ->
            vbox $
                [ hbox2margin start (format first) ]
                ++ (List.map (hbox2margin (text mid) . format) rest) ++
                if hasSize end then [ end ] else []


hbox2 :: Box' -> Box' -> Box'
hbox2 (Box' a _ ae) (Box' b _ be) =
    Box' (a <> b) 0 (ae || be)


hbox2margin :: Box' -> Box' -> Box'
hbox2margin (Box' a aMargin ae) (Box' b bMargin be) =
    Box' (a <> b) (max aMargin bMargin) (ae || be)


hbox :: [Box'] -> Box'
hbox =
    foldl hbox2 empty


hboxlist :: String -> String -> String -> (a -> Box') -> [a] -> Box'
hboxlist start mid end format items =
    hbox $
        [ text start ]
        ++ (List.map format items |> List.intersperse (text mid)) ++
        [ text end ]


hjoin :: Box' -> [Box'] -> Box'
hjoin sep list =
    hbox (List.intersperse sep list)


indent' :: Int -> Box' -> Box'
indent' i child =
    hbox2margin (hspace i) child


margin :: Int -> Box' -> Box'
margin m (Box' child _ ae) =
    Box' child m ae


width :: Box' -> Int
width (Box' b _ _) =
    B.cols b


height :: Box' -> Int
height (Box' b _ _) =
    B.rows b


render :: Box' -> String
render (Box' child _ _) =
    B.render child
