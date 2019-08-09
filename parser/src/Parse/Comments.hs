module Parse.Comments where

import Parse.Whitespace
import Parse.IParser
import AST.V0_16



commented :: IParser a -> IParser (Commented a)
commented inner =
    Commented <$> whitespace <*> inner <*> whitespace


postCommented :: IParser a -> IParser (PostCommented a)
postCommented a =
    (,) <$> a <*> whitespace


preCommented :: IParser a -> IParser (PreCommented a)
preCommented a =
    (,) <$> whitespace <*> a


withEol :: IParser a -> IParser (WithEol a)
withEol a =
    do
        (result, multiline) <- trackNewline a
        case multiline of
            SplitAll -> return $ WithEol result Nothing
            JoinAll -> WithEol result <$> restOfLine
