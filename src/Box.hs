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


len :: Line -> Int
len l =
    case l of
        Text s -> length s
        Row ls -> sum $ map len ls
        Space -> 1


data Box
    = Stack Line [Line]
    -- | Margin Int
    -- | Empty


blankLine :: Box
blankLine =
    line $ literal ""


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
        (\l -> row [ pref, l ])
        (\l -> row [ row $ replicate (len pref) space, l ])


elmGroup :: Bool -> String -> String -> String -> Bool -> [Box] -> Box
elmGroup innerSpaces left sep right forceMultiline children =
    case (forceMultiline, allSingles children) of
        (_, Just []) ->
            line $ row [punc left, punc right]
        (False, Just ls) ->
            line $ row $ concat
                [ if innerSpaces then [punc left, space] else [punc left]
                , List.intersperse (row [punc sep, space]) ls
                , if innerSpaces then [space, punc right] else [punc right]
                ]
        _ ->
            case children of
                [] ->
                    line $ row [ punc left, punc right]
                (first:rest) ->
                    stack $
                        (prefix (row [punc left, space]) first)
                        : (map (prefix $ row [punc sep, space]) rest)
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


text :: String -> Box'
text s =
    Box' (B.text s) 0 (s /= "")


vbox2 :: Box' -> Box' -> Box'
vbox2 (Box' a aMargin ae) (Box' b bMargin be) =
    Box' (a // B.emptyBox aMargin 0 // b) bMargin (ae || be)


vbox :: [Box'] -> Box'
vbox =
    foldl vbox2 empty


hbox2 :: Box' -> Box' -> Box'
hbox2 (Box' a _ ae) (Box' b _ be) =
    Box' (a <> b) 0 (ae || be)


hbox :: [Box'] -> Box'
hbox =
    foldl hbox2 empty


render :: Box' -> String
render (Box' child _ _) =
    B.render child
