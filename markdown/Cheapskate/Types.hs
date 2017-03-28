{-# LANGUAGE DeriveDataTypeable #-}
module Cheapskate.Types where
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Map as M
import Data.Data

-- | Structured representation of a document.  The 'Options' affect
-- how the document is rendered by `toHtml`.
data Doc = Doc Options Blocks
           deriving (Show, Data, Typeable)

-- | Block-level elements.
data Block = Para Inlines
           | Header Int Inlines
           | Blockquote Blocks
           | List Bool ListType [Blocks]
           | CodeBlock CodeAttr Text
           | HtmlBlock Text
           | HRule
           | ReferencesBlock [(Text, Text, Text)]
           deriving (Show, Data, Typeable)

-- | Attributes for fenced code blocks.  'codeLang' is the
-- first word of the attribute line, 'codeInfo' is the rest.
data CodeAttr = CodeAttr { codeLang :: Text, codeInfo :: Text }
              deriving (Show, Data, Typeable)

data ListType = Bullet Char | Numbered NumWrapper Int deriving (Eq,Show,Data,Typeable)
data NumWrapper = PeriodFollowing | ParenFollowing deriving (Eq,Show,Data,Typeable)

-- | Simple representation of HTML tag.
data HtmlTagType = Opening Text | Closing Text | SelfClosing Text deriving (Show, Data, Typeable)

-- We operate with sequences instead of lists, because
-- they allow more efficient appending on to the end.
type Blocks = Seq Block

-- | Inline elements.
data Inline = Str Text
            | Space
            | SoftBreak
            | LineBreak
            | Emph Inlines
            | Strong Inlines
            | Code Text
            | Link Inlines LinkTarget {- URL -} Text {- title -}
            | Image Inlines Text {- URL -} Text {- title -}
            | Entity Text
            | RawHtml Text
            deriving (Show, Data, Typeable)


data LinkTarget
    = Url Text
    | Ref Text
    deriving (Show, Data)


type Inlines = Seq Inline

type ReferenceMap = M.Map Text (Text, Text)

-- | Rendering and parsing options.
data Options = Options{
    sanitize           :: Bool  -- ^ Sanitize raw HTML, link/image attributes
  , allowRawHtml       :: Bool  -- ^ Allow raw HTML (if false it gets escaped)
  , preserveHardBreaks :: Bool  -- ^ Preserve hard line breaks in the source
  , debug              :: Bool  -- ^ Print container structure for debugging
  }
  deriving (Show, Data, Typeable)
