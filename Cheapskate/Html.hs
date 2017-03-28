{-# LANGUAGE CPP, OverloadedStrings #-}
module Cheapskate.Html (renderDoc, renderBlocks, renderInlines) where
import Cheapskate.Types
import Data.Text (Text)
import Data.Char (isDigit, isHexDigit, isAlphaNum)
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as BT
import Text.Blaze.Html hiding(contents)
import Data.Monoid
#if !(MIN_VERSION_base(4,8,0))
import Data.Foldable (foldMap)
#endif
import Data.Foldable (toList)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.List (intersperse)
import Text.HTML.SanitizeXSS (sanitizeBalance)

-- | Render a markdown document as 'Html'.  (This can be turned
-- into a 'Text' or 'ByteString' using a renderer from the @blaze-html@
-- library.)
renderDoc :: Doc -> Html
renderDoc (Doc opts body) = mbsanitize $ (renderBlocks opts body <> "\n")
  where mbsanitize = if sanitize opts
                        then preEscapedToMarkup . sanitizeBalance .
                             TL.toStrict . BT.renderHtml
                        else id
  -- note: less efficient to do this at the whole document level,
  -- rather than on individual raw html bits and attributes, but
  -- this is needed for cases where open tags in one raw HTML
  -- section are balanced by close tags in another.

-- Render a sequence of blocks as HTML5.  Currently a single
-- newline is used between blocks, and a newline is used as a
-- separator e.g. for list items. These can be changed by adjusting
-- nl and blocksep.  Eventually we probably want these as parameters
-- or options.
renderBlocks :: Options -> Blocks -> Html
renderBlocks opts = mconcat . intersperse blocksep . map renderBlock . toList
  where renderBlock :: Block -> Html
        renderBlock (Header n ils)
          | n >= 1 && n <= 6 = ([H.h1,H.h2,H.h3,H.h4,H.h5,H.h6] !! (n - 1))
                                  $ renderInlines opts ils
          | otherwise        = H.p (renderInlines opts ils)
        renderBlock (Para ils) = H.p (renderInlines opts ils)
        renderBlock (HRule) = H.hr
        renderBlock (Blockquote bs) = H.blockquote $ nl <> renderBlocks opts bs <> nl
        renderBlock (CodeBlock attr t) =
          if T.null (codeLang attr)
             then base
             else base ! A.class_ (toValue' $ codeLang attr)
          where base = H.pre $ H.code $ toHtml (t <> "\n")
          -- add newline because Markdown.pl does
        renderBlock (List tight (Bullet _) items) =
          H.ul $ nl <> mapM_ (li tight) items
        renderBlock (List tight (Numbered _ n) items) =
          if n == 1 then base else base ! A.start (toValue n)
          where base = H.ol $ nl <> mapM_ (li tight) items
        renderBlock (HtmlBlock raw) =
          if allowRawHtml opts
             then H.preEscapedToMarkup raw
             else toHtml raw
        li :: Bool -> Blocks -> Html  -- tight list handling
        li True = (<> nl) . H.li . mconcat . intersperse blocksep .
                      map renderBlockTight . toList
        li False = toLi
        renderBlockTight (Para zs) = renderInlines opts zs
        renderBlockTight x         = renderBlock x
        toLi x = (H.li $ renderBlocks opts x) <> nl
        nl = "\n"
        blocksep = "\n"

-- Render a sequence of inlines as HTML5.
renderInlines :: Options -> Inlines -> Html
renderInlines opts = foldMap renderInline
  where renderInline :: Inline -> Html
        renderInline (Str t) = toHtml t
        renderInline Space   = " "
        renderInline SoftBreak
          | preserveHardBreaks opts = H.br <> "\n"
          | otherwise               = "\n"
          -- this preserves the line breaks in the
          -- markdown document; replace with " " if this isn't wanted.
        renderInline LineBreak = H.br <> "\n"
        renderInline (Emph ils) = H.em $ renderInlines opts ils
        renderInline (Strong ils) = H.strong $ renderInlines opts ils
        renderInline (Code t) = H.code $ toHtml t
        renderInline (Link ils url tit) =
          if T.null tit then base else base ! A.title (toValue' tit)
          where base = H.a ! A.href (toValue' url) $ renderInlines opts ils
        renderInline (Image ils url tit) =
          if T.null tit then base else base ! A.title (toValue' tit)
          where base = H.img ! A.src (toValue' url)
                             ! A.alt (toValue
                                $ BT.renderHtml $ renderInlines opts ils)
        renderInline (Entity t) = H.preEscapedToMarkup t
        renderInline (RawHtml t) =
          if allowRawHtml opts
             then H.preEscapedToMarkup t
             else toHtml t

toValue' :: Text -> AttributeValue
toValue' = preEscapedToValue . gentleEscape . T.unpack

-- preserve existing entities
gentleEscape :: String -> String
gentleEscape [] = []
gentleEscape ('"':xs) = "&quot;" ++ gentleEscape xs
gentleEscape ('\'':xs) = "&#39;" ++ gentleEscape xs
gentleEscape ('&':'#':x:xs)
  | x == 'x' || x == 'X' =
  case span isHexDigit xs of
       (ys,';':zs) | not (null ys) && length ys < 6 ->
         '&':'#':x:ys ++ ";" ++ gentleEscape zs
       _ -> "&amp;#" ++ (x : gentleEscape xs)
gentleEscape ('&':'#':xs) =
  case span isDigit xs of
       (ys,';':zs) | not (null ys) && length ys < 6 ->
         '&':'#':ys ++ ";" ++ gentleEscape zs
       _ -> "&amp;#" ++ gentleEscape xs
gentleEscape ('&':xs) =
  case span isAlphaNum xs of
       (ys,';':zs) | not (null ys) && length ys < 11 ->
         '&':ys ++ ";" ++ gentleEscape zs
       _ -> "&amp;" ++ gentleEscape xs
gentleEscape (x:xs) = x : gentleEscape xs
