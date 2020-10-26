module Parse.Common
    ( sectionedGroup, pair
    , commented, preCommented, postCommented, withEol
    , checkMultiline
    ) where

import AST.V0_16
import Text.Parsec
import Parse.Helpers
import Parse.Whitespace
import Parse.IParser
import Parse.Comments


--
-- Structure
--


pair :: IParser a -> IParser sep -> IParser b -> IParser (Pair a b)
pair a sep b =
    checkMultiline $ Pair <$> postCommented a <* sep <*> preCommented b


sectionedGroup :: IParser a -> IParser (Sequence a, Comments)
sectionedGroup term =
    let
        step leading terms =
            do
                pre <- whitespace
                (C eol first) <- withEol term
                preSep <- whitespace
                hasMore <- choice [ comma *> return True, return False ]
                if hasMore
                    then step preSep (C (leading, pre, eol) first : terms)
                    else return (Sequence $ reverse (C (leading, pre, eol) first : terms), preSep)
    in
        choice
            [ try $ step [] []
            , (,) (Sequence []) <$> whitespace
            ]


--
-- Other helpers
--


checkMultiline :: IParser (ForceMultiline -> a) -> IParser a
checkMultiline inner =
    do
        (a, multiline) <- trackNewline inner
        return $ a (ForceMultiline $ multilineToBool multiline)
