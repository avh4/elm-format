module ElmFormat.Cli (main) where

import ElmFormat.World
import qualified ElmFormat.World as World


main :: World m => (String -> Either String a) -> (a -> String) -> [String] -> m ()
main parse render args =
    do
        input <- World.readFile "file.elm"

        case parse input of
            Left message ->
                error $ show message

            Right ast ->
                World.writeFile "file.elm" (render ast)
