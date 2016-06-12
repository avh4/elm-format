module ElmFormat.Cli (main) where

import ElmFormat.World
import qualified ElmFormat.World as World


main :: World m => (String -> Either String a) -> (a -> String) -> [String] -> m ()
main parse render args =
    do
        -- TODO: read filename from args
        input <- World.readFile "good.elm"

        case parse input of
            Left message ->
                error $ show message

            Right ast ->
                World.writeFile "good.elm" (render ast)
