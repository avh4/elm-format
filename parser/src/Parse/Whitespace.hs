module Parse.Whitespace where

import AST.V0_16
import qualified Cheapskate.Types as Markdown
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Parse.IParser
import qualified Parse.Markdown as Markdown
-- import qualified Reporting.Error.Syntax as Syntax
import Parse.Primitives (try)
import Parse.Primitives.Internals (Parser(..), State(..))
import Parse.ParsecAdapter (string, (<|>), many, many1, choice, option, char, eof, lookAhead, notFollowedBy, anyWord8)
-- import Text.Parsec hiding (newline, spaces, State)


padded :: IParser a -> IParser (Comments, a, Comments)
padded p =
  do  pre <- whitespace
      out <- p
      post <- whitespace
      return (pre, out, post)


spaces :: IParser Comments
spaces =
  let
      blank = string " " >> return []
      comment = ((\x -> [x]) <$> multiComment)
      space =
        blank
        <|> (const [CommentTrickOpener] <$> (try $ string "{--}"))
        <|> comment
        -- <?> Syntax.whitespace
  in
      concat <$> many1 space


forcedWS :: IParser Comments
forcedWS =
  choice
    [ (++) <$> spaces <*> (concat <$> many nl_space)
    , concat <$> many1 nl_space
    ]
  where
    nl_space =
      try ((++) <$> (concat <$> many1 newline) <*> option [] spaces)


-- Just eats whitespace until the next meaningful character.
dumbWhitespace :: IParser Comments
dumbWhitespace =
  concat <$> many (spaces <|> newline)


whitespace' :: IParser (Bool, Comments)
whitespace' =
  option (False, []) ((,) True <$> forcedWS)


whitespace :: IParser Comments
whitespace =
  snd <$> whitespace'


freshLine :: IParser Comments
freshLine =
      concat <$> (try ((++) <$> many1 newline <*> many space_nl) <|> try (many1 space_nl)) -- <?> Syntax.freshLine
  where
    space_nl = try $ (++) <$> spaces <*> (concat <$> many1 newline)


newline :: IParser Comments
newline =
    (simpleNewline >> return []) <|> ((\x -> [x]) <$> lineComment) -- <?> Syntax.newline


simpleNewline :: IParser ()
simpleNewline =
  do  _ <- try (string "\r\n") <|> string "\n"
      return ()


trackNewline :: IParser a -> IParser (a, Multiline)
trackNewline (Parser parser) =
    Parser $ \state@(State _ _ _ _ row _ _) cok _ eok err ->
        let
            cok' a newState@(State _ _ _ _ newRow _ _) e =
                if newRow > row
                    then cok (a, SplitAll) newState e
                    else cok (a, JoinAll) newState e

            eok' a newState e =
                -- Nothing was consumed, so there cannot have been a newline
                eok (a, JoinAll) newState e

        in
        parser state cok' err eok' err


lineComment :: IParser Comment
lineComment =
  do  _ <- try (string "--")
      choice
        [ const CommentTrickCloser
            <$> try (char '}' >> many (char ' ') >> (simpleNewline <|> eof))
        , do
            (comment, ()) <-
              anyUntil $ simpleNewline <|> eof
            return $ LineComment comment
        ]


restOfLine :: IParser (Maybe String)
restOfLine =
    many (char ' ') *>
        choice
            [ Just . fst <$> (try (string "--") *> (anyUntil $ (lookAhead simpleNewline) <|> eof))
            , return Nothing
            ]


docComment :: IParser String
docComment =
  do  _ <- try (string "{-|")
      _ <- many (string " ")
      closeComment False


docCommentAsMarkdown :: IParser Markdown.Blocks
docCommentAsMarkdown =
    Markdown.parse <$> docComment


multiComment :: IParser Comment
multiComment =
  do  _ <- try (string "{-" <* notFollowedBy (string "|") )
      isCommentTrick <-
        choice
          [ char '-' >> return True
          , return False
          ]
      _ <- many (string " ")
      b <- closeComment False
      return $
        if isCommentTrick then
          CommentTrickBlock b
        else
          BlockComment $ trimIndent $ lines b
  where
      trimIndent [] = []
      trimIndent (l1:ls) =
          let
              leadingIndents =
                  map fst $ filter (uncurry (/=))
                      $ map (\l -> (length $ takeWhile Char.isSpace l, length l)) ls

              depth =
                  case leadingIndents of
                      [] -> 0
                      _ -> minimum leadingIndents
          in
              l1 : map (drop depth) ls


closeComment :: Bool -> IParser String
closeComment keepClosingPunc =
  uncurry (++) <$>
    anyUntil
      (choice
        [ try ((\a b -> if keepClosingPunc then concat (a ++ [b]) else "") <$> many (string " ") <*> string "-}") -- <?> "the end of a comment -}"
        , concat <$> sequence [ try (string "{-"), closeComment True, closeComment keepClosingPunc]
        ])


anyUntil :: IParser a -> IParser (String, a)
anyUntil end =
  go []
  where
    next pre =
      do
        nextByte <- anyWord8
        go (nextByte : pre)

    go pre =
      ((,) (Text.unpack . decodeUtf8 . ByteString.pack $ reverse pre) <$> end) <|> next pre
