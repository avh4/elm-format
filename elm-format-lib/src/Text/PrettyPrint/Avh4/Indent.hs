module Text.PrettyPrint.Avh4.Indent (Indent, tab,spaces,width) where


spacesInTab :: Word
spacesInTab = 4


{-| `Indent` represents an indentation level,
and the operator `<>` can be used to combine two indentations side-by-side, accounting for the tab size.

Each `Indent` can be thought of as:
one or more TABs, followed by zero to three SPACEs.

Combining two indents can be thought of as
typing the first and then the second sequence of
TABs and SPACEs in a word processor.

For example:

    [TAB] <> [TAB]     ==  [TAB][TAB]
    [TAB] <> ...       ==  [TAB]...
    [TAB] <> [TAB]...  ==  [TAB][TAB]...
     <> ...            ==  ...
    [TAB].. <> [TAB]   ==  [TAB][TAB]
    .. <> .            ==  ...
    .. <> ..           ==  [TAB]

-}
newtype Indent =
    Indent [Word]
    deriving (Semigroup, Monoid, Show)

instance Eq Indent where
    a == b =
        width' a == width' b


tab :: Indent
tab = Indent [spacesInTab]


spaces :: Word -> Indent
spaces = Indent . pure


width :: Num n => Indent -> n
width = fromIntegral . width'


width' :: Indent -> Word
width' (Indent is) =
    foldl combine 0 is


combine :: Word -> Word -> Word
combine pos i =
    if i < spacesInTab
        -- The right side starts with spaces (and no TABs),
        -- so just add everything together.
        then pos + i

        -- The right side starts with at least one TAB,
        -- so remove the trailing spaces from the left.
        else pos - (pos `mod` spacesInTab) + i
