module ElmFormat.Cli (main) where

import ElmFormat.World

import ElmVersion (ElmVersion)

import qualified Flags as Flags
import qualified ElmFormat.World as World


main :: World m => ElmVersion -> String -> (String -> Either String a) -> (a -> String) -> [String] -> m ()
main defaultElmVersion elmFormatVersion parse render args =
    do
        let flags = Flags.parse' defaultElmVersion elmFormatVersion args

        case flags of
            Right flags ->
                doIt defaultElmVersion elmFormatVersion parse render flags


doIt defaultElmVersion elmFormatVersion parse render flags =
    do
        progName <- World.getProgName
        case Flags._input flags of
            [] ->
                World.putStrLn $ Flags.usage defaultElmVersion progName elmFormatVersion

            [path] ->
                do
                    input <- World.readFile path

                    case parse input of
                        Left message ->
                            error $ show message

                        Right ast ->
                            World.writeFile path (render ast)
