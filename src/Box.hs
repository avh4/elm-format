{-# OPTIONS_GHC -Wall #-}
module Box
  ( Line, identifier, keyword, punc, literal, row, space
  , Box(SingleLine), blankLine, line, mustBreak, stack1, andThen
  , isLine, allSingles
  , indent, prefix, addSuffix
  , render
  ) where

import Elm.Utils ((|>))

import qualified Data.Text as T


{-
A line is ALWAYS just one line.

Space is self-explanatory,
  Tab aligns to the nearest multiple of 4 spaces,
  Text brings any string into the data structure,
  Row joins more of these elements onto one line.
-}
data Line
    = Text T.Text
    | Row [Line]
    | Space
    | Tab


identifier :: String -> Line
identifier =
    Text . T.pack


keyword :: String -> Line
keyword =
    Text . T.pack


punc :: String -> Line
punc =
    Text . T.pack


literal :: String -> Line
literal =
    Text . T.pack


-- join more Line elements into one
row :: [Line] -> Line
row =
    Row


space :: Line
space =
    Space


{-
Box contains Lines (at least one - can't be empty).
Box either:
  - can appear in the middle of a line
      (Stack someLine [], thus can be joined without problems), or
  - has to appear on its own
      (Stack someLine moreLines OR MustBreak someLine).

MustBreak is only used for `--` comments.

Stack contains two or more lines.

Sometimes (see `prefix`) the first line of Stack
  gets different treatment than the other lines.
-}
data Box
    = SingleLine Line
    | Stack Line Line [Line]
    | MustBreak Line


blankLine :: Box
blankLine =
    line $ literal ""


line :: Line -> Box
line l =
    SingleLine l


mustBreak :: Line -> Box
mustBreak l =
    MustBreak l


stack' :: Box -> Box -> Box
stack' b1 b2 =
    let
        (line1first, line1rest) = destructure b1
        (line2first, line2rest) = destructure b2
    in case line1rest ++ line2first:line2rest of
        [] ->
            error "the list will contain at least line2first"
        first : rest ->
            Stack line1first first rest


andThen :: [Box] -> Box -> Box
andThen rest first =
    foldl stack' first rest


stack1 :: [Box] -> Box
stack1 children =
    case children of
        [] ->
            error "stack1: empty structure"
        [first] ->
            first
        boxes ->
            foldl1 stack' boxes


mapLines :: (Line -> Line) -> Box -> Box
mapLines fn =
    mapFirstLine fn fn


mapFirstLine :: (Line -> Line) -> (Line -> Line) -> Box -> Box
mapFirstLine firstFn restFn b =
    case b of
        SingleLine l1 ->
            SingleLine (firstFn l1)
        Stack l1 l2 ls ->
            Stack (firstFn l1) (restFn l2) (map restFn ls)
        MustBreak l1 ->
            MustBreak (firstFn l1)


indent :: Box -> Box
indent =
    mapLines (\l -> row [Tab, l])


isLine :: Box -> Either Box Line
isLine b =
    case b of
        SingleLine l ->
            Right l
        _ ->
            Left b


destructure :: Box -> (Line, [Line])
destructure b =
    case b of
        SingleLine l1 ->
            (l1, [])
        Stack l1 l2 rest ->
            (l1, l2 : rest)
        MustBreak l1 ->
            (l1, [])


allSingles :: [Box] -> Either [Box] [Line]
allSingles boxes =
    case sequence $ map isLine boxes of
        Right lines' ->
            Right lines'
        _ ->
            Left boxes


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
    let
        prefixLength = lineLength 0 pref
        paddingSpaces = replicate prefixLength space
        padLineWithSpaces l = row [ row paddingSpaces, l ]
        addPrefixToLine l = row [ pref, l ]
    in
      mapFirstLine addPrefixToLine padLineWithSpaces


addSuffix :: Line -> Box -> Box
addSuffix suffix b =
    case destructure b of
        (l,[]) ->
            line $ row [ l, suffix ]
        (l1,ls) ->
            line l1
                |> andThen (map line $ init ls)
                |> andThen [ line $ row [ last ls, suffix ] ]


renderLine :: Int -> Line -> T.Text
renderLine startColumn line' =
    case line' of
        Text text ->
            text
        Space ->
            T.singleton ' '
        Tab ->
            T.pack $ replicate (tabLength startColumn) ' '
        Row lines' ->
            renderRow startColumn lines'


render :: Box -> T.Text
render box' =
    case box' of
        SingleLine line' ->
            T.snoc (renderLine 0 line') '\n'
        Stack l1 l2 rest ->
            T.unlines $ map (renderLine 0) (l1 : l2 : rest)
        MustBreak line' ->
            T.snoc (renderLine 0 line') '\n'


-- TODO couldn't we just run renderLine and get the length of the resulting string?
lineLength :: Int -> Line -> Int
lineLength startColumn line' =
   startColumn +
      case line' of
         Text string -> T.length string
         Space -> 1
         Tab -> tabLength startColumn
         Row lines' -> rowLength startColumn lines'


initRow :: Int -> (T.Text, Int)
initRow startColumn =
  (T.empty, startColumn)


spacesInTab :: Int
spacesInTab =
  2


spacesToNextTab :: Int -> Int
spacesToNextTab startColumn =
  startColumn `mod` spacesInTab

tabLength :: Int -> Int
tabLength startColumn =
  spacesInTab - (spacesToNextTab startColumn)

{-
What happens here is we take a row and start building its contents
  along with the resulting length of the string. We need to have that
  because of Tabs, which need to be passed the current column in arguments
  in order to determine how many Spaces are they going to span.
  (See `tabLength`.)

So for example if we have a Box [Space, Tab, Text "abc", Tab, Text "x"],
  it goes like this:

string      | column | todo
""          | 0      | [Space, Tab, Text "abc", Tab, Text "x"]
" "         | 1      | [Tab, Text "abc", Tab, Text "x"]
"    "      | 4      | [Text "abc", Tab, Text "x"]
"    abc"   | 7      | [Tab, Text "x"]
"    abc "  | 8      | [Text "x"]
"    abc x" | 9      | []

Thus we get the result string with correctly rendered Tabs.

The (T.Text, Int) type here means the (string, column) from the table above.

Then we just need to do one final modification to get from endColumn to resultLength,
  which is what we are after in the function `rowLength`.
-}
renderRow' :: Int -> [Line] -> (T.Text, Int)
renderRow' startColumn lines' =
  (result, resultLength)
  where
    (result, endColumn) = foldl addLine (initRow startColumn) lines'
    resultLength = endColumn - startColumn

{-
A step function for renderRow'.

addLine (" ",1) Tab == ("    ",4)
-}
addLine :: (T.Text, Int) -> Line -> (T.Text, Int)
addLine (string, startColumn') line' =
  (newString, newStartColumn)
  where
    newString = T.append string $ renderLine startColumn' line'
    newStartColumn = lineLength startColumn' line'

-- Extract the final string from renderRow'
renderRow :: Int -> [Line] -> T.Text
renderRow startColumn lines' =
  fst $ renderRow' startColumn lines'

-- Extract the final length from renderRow'
rowLength :: Int -> [Line] -> Int
rowLength startColumn lines' =
  snd $ renderRow' startColumn lines'
