{-# LANGUAGE DeriveGeneric #-}
module ElmFormat.AST.PublicAST.Comment (Comment(..), mkComment, CommentDisplay(..), CommentType(..)) where

import ElmFormat.AST.PublicAST.Core
import qualified AST.V0_16 as AST
import qualified Data.List as List


data Comment
    = Comment
        { text :: String
        , display :: CommentDisplay
        }
    deriving (Show)

mkComment :: AST.Comment -> Comment
mkComment = \case
    AST.BlockComment lines ->
        Comment
            (List.intercalate "\n" lines)
            (CommentDisplay BlockComment)

    AST.LineComment string ->
        Comment
            string
            (CommentDisplay BlockComment)

    AST.CommentTrickOpener ->
        error "TODO: CommentTrickOpener"

    AST.CommentTrickCloser ->
        error "TODO: CommentTrickCloser"

    AST.CommentTrickBlock _ ->
        error "TODO: CommentTrickCloser"


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

newtype CommentDisplay =
    CommentDisplay
        { commentType :: CommentType
        }
    deriving (Show, Generic)

instance ToJSON CommentDisplay where
    toEncoding = genericToEncoding defaultOptions


data CommentType
    = BlockComment
    | LineComment
    deriving (Show, Generic)

instance ToJSON CommentType where
    toEncoding = genericToEncoding defaultOptions
