{-# OPTIONS_GHC -Wall #-}
module Box where

import Elm.Utils ((|>))

import qualified Data.List as List


{-
A line is ALWAYS just one line.

Space and Tab are self-explanatory,
  Text brings any string into the data structure,
  Row joins more of these elements onto one line.
-}
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


-- join more Line elements into one
row :: [Line] -> Line
row =
    Row


space :: Line
space =
    Space


lineLength :: Int -> Line -> Int
lineLength startColumn l =
   startColumn +
      case l of
         Text s -> length s
         Row ls -> sum $ map (lineLength 0) ls
         Space -> 1
         Tab -> 0


{-
Box contains Lines (at least one - can't be empty).
Box either:
  - can appear in the middle of a line
      (Stack someLine [], thus can be joined without problems), or
  - has to appear on its own
      (Stack someLine moreLines OR MustBreak someLine).

MustBreak is only used for `--` comments.

Stack doesn't allow zero-line stacks.

Sometimes (see `prefix`) the first line of Stack
  gets different treatment than the other lines.
-}
data Box
    = Stack Line [Line]
    | MustBreak Line
    -- | Margin Int


blankLine :: Box
blankLine =
    line $ literal ""


line :: Line -> Box
line l =
    Stack l []


mustBreak :: Line -> Box
mustBreak l =
    MustBreak l


stack' :: Box -> Box -> Box
stack' b1 b2 =
    let
        split b =
            case b of
                Stack l1 ln ->
                    (l1, ln)
                MustBreak l1 ->
                    (l1, [])
        (l11, l1n) = split b1
        (l21, l2n) = split b2
    in
        Stack l11 (l1n ++ (l21:l2n))


stack :: [Box] -> Box
stack children =
    case children of
        -- [] -> Stack (Text "*****") []
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
        MustBreak l1 ->
            MustBreak (firstFn l1)


indent :: Box -> Box
indent =
    mapLines (\l -> row [Tab, l])


isLine :: Box -> Either Box Line
isLine b =
    case b of
        Stack l [] ->
            Right l
        _ ->
            Left b


-- TODO: replace with isLine?
destructure :: Box -> (Line, [Line])
destructure b =
    case b of
        Stack first rest ->
            (first, rest)
        MustBreak first ->
            (first, [])


allSingles :: [Box] -> Either [Box] [Line]
allSingles boxes =
    case sequence $ map isLine boxes of
        Right lines' ->
            Right lines'
        _ ->
            Left boxes


allSingles2 :: Box -> Box -> Either (Box, Box) (Line, Line)
allSingles2 a b =
    case (isLine a, isLine b) of
        (Right a', Right b') ->
            Right (a', b')

        _ ->
            Left (a, b)


elmApplication :: Box -> [Box] -> Box
elmApplication first rest =
    case allSingles (first:rest) of
        Right ls ->
            ls
                |> List.intersperse space
                |> row
                |> line
        _ ->
            stack
                $ first : (map indent rest)


{-
Add the prefix to the first line,
pad the other lines with spaces of the same length

EXAMPLE:
abcde
xyz
----->
myPrefix abcde
         xyz
-}
prefix :: Line -> Box -> Box
prefix pref =
    mapFirstLine
        addPrefixToLine
        padLineWithSpaces
    where
        addPrefixToLine l = row [ pref, l ]
        padLineWithSpaces l = row [ row paddingSpaces, l ]

        prefixLength = lineLength 0 pref
        paddingSpaces = replicate prefixLength space


elmGroup :: Bool -> String -> String -> String -> Bool -> [Box] -> Box
elmGroup innerSpaces left sep right forceMultiline children =
    case (forceMultiline, allSingles children) of
        (_, Right []) ->
            line $ row [punc left, punc right]
        (False, Right ls) ->
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

renderLine :: Line -> String
renderLine (Text text) =
    text
renderLine (Row ls) =
    concat $ map renderLine ls
renderLine Space =
    " "
renderLine Tab =
    "\t"

render :: Box -> String
render (Stack firstLine moreLines) =
    unlines $ map renderLine (firstLine : moreLines)
render (MustBreak l) =
    renderLine l ++ "\n"
