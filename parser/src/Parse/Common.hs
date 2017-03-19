module Parse.Common
    ( sectionedGroup, pair
    , commented, preCommented, postCommented
    , checkMultiline
    ) where

import AST.V0_16
import Text.Parsec
import Parse.Helpers


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
                first <- term
                eol <- restOfLine
                preSep <- whitespace
                hasMore <- choice [ comma *> return True, return False ]
                if hasMore
                    then step preSep ((leading, (pre, (first, eol))) : terms)
                    else return (reverse $ (leading, (pre, (first, eol))) : terms, preSep)
    in
        choice
            [ try $ step [] []
            , (,) [] <$> whitespace
            ]



--
-- Comments
--


commented :: IParser a -> IParser (Commented a)
commented inner =
    Commented <$> whitespace <*> inner <*> whitespace


postCommented :: IParser a -> IParser (PostCommented a)
postCommented a =
    (,) <$> a <*> whitespace


preCommented :: IParser a -> IParser (PreCommented a)
preCommented a =
    (,) <$> whitespace <*> a



--
-- Other helpers
--


checkMultiline :: IParser (ForceMultiline -> a) -> IParser a
checkMultiline inner =
    do
        pushNewlineContext
        a <- inner
        multiline <- popNewlineContext
        return $ a (ForceMultiline multiline)
