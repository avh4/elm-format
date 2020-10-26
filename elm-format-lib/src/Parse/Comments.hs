module Parse.Comments where

import Parse.Whitespace
import Parse.IParser
import AST.V0_16



commented :: IParser a -> IParser (C2 l1 l2 a)
commented inner =
    (\c1 a c2 -> C (c1, c2) a) <$> whitespace <*> inner <*> whitespace


postCommented :: IParser a -> IParser (C1 l a)
postCommented a =
    flip C <$> a <*> whitespace


preCommented :: IParser a -> IParser (C1 l a)
preCommented a =
    C <$> whitespace <*> a


withEol :: IParser a -> IParser (C0Eol a)
withEol a =
    do
        (result, multiline) <- trackNewline a
        case multiline of
            SplitAll -> return $ C Nothing result
            JoinAll -> flip C result <$> restOfLine
