{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cheapskate (markdown,
                   def,
                   walk,
                   walkM,
                   module Cheapskate.Types
                   ) where
import Cheapskate.Types
import Cheapskate.Parse
import Cheapskate.Html
import Data.Default (def)
import Data.Data
import Data.Generics.Uniplate.Data
import Text.Blaze.Html (ToMarkup(..))

instance ToMarkup Doc
  where toMarkup = renderDoc

-- | Apply a transformation bottom-up to every node of a parsed document.
-- This can be used, for example, to transform specially marked code blocks
-- to highlighted code or images.  Here is a simple example that promotes
-- the levels of headers:
--
-- > promoteHeaders :: Doc -> Doc
-- > promoteHeaders = walk promoteHeader
-- >   where promoteHeader (Header n ils) = Header (n+1) ils
-- >         promoteHeader x              = x
walk :: (Data a, Data b) => (a -> a) -> (b -> b)
walk = transformBi

-- | Monadic version of 'walk'.
walkM :: (Data a, Data b, Monad m) => (a -> m a) -> (b -> m b)
walkM = transformBiM
