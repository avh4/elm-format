{-# LANGUAGE DeriveGeneric #-}
module ElmFormat.AST.PublicAST.Comment (Comment(..), mkComment, fromComment, CommentDisplay(..), CommentType(..)) where

import ElmFormat.AST.PublicAST.Core
import qualified AST.V0_16 as AST
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text


data Comment
    = Comment
        { text :: Text
        , display :: CommentDisplay
        }
    deriving (Show)

mkComment :: AST.Comment -> Comment
mkComment = \case
    AST.BlockComment lines ->
        Comment
            (Text.pack $ List.intercalate "\n" lines)
            (CommentDisplay BlockComment)

    AST.LineComment string ->
        Comment
            (Text.pack string)
            (CommentDisplay LineComment)

    AST.CommentTrickOpener ->
        error "TODO: CommentTrickOpener"

    AST.CommentTrickCloser ->
        error "TODO: CommentTrickCloser"

    AST.CommentTrickBlock _ ->
        error "TODO: CommentTrickCloser"

fromComment :: Comment -> AST.Comment
fromComment = \case
    Comment text (CommentDisplay BlockComment) ->
        AST.BlockComment (Text.unpack <$> Text.splitOn "\n" text)

    Comment text (CommentDisplay LineComment) ->
        AST.LineComment (Text.unpack text)


instance ToJSON Comment where
    toJSON = undefined
    toEncoding = pairs . toPairs

instance ToPairs Comment where
    toPairs = \case
        Comment text display ->
            mconcat
                [ type_ "Comment"
                , "text" .= text
                , "display" .= display
                ]

instance FromJSON Comment where
    parseJSON = withObject "Comment" $ \obj ->
        Comment
            <$> obj .: "text"
            <*> obj .:? "display" .!= CommentDisplay LineComment


newtype CommentDisplay =
    CommentDisplay
        { commentType :: CommentType
        }
    deriving (Show, Generic)

instance ToJSON CommentDisplay where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CommentDisplay


data CommentType
    = BlockComment
    | LineComment
    deriving (Show, Generic)

instance ToJSON CommentType where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CommentType
