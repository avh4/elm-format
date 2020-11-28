module Parse.Markdown (parse) where

import qualified Cheapskate.Parse
import qualified Cheapskate.Types as Markdown
import qualified Data.Text as Text
import Elm.Utils ((|>))


parse :: String -> Markdown.Blocks
parse source =
    source
        |> Text.pack
        |> Cheapskate.Parse.markdown
            (Markdown.Options
                { Markdown.sanitize = True
                , Markdown.allowRawHtml = True
                , Markdown.preserveHardBreaks = True
                , Markdown.debug = False
                }
            )
        |> (\(Markdown.Doc _ source) -> source)
