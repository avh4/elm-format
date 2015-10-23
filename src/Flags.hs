{-# OPTIONS_GHC -Wall #-}
module Flags where

import Data.Monoid ((<>))
import Data.Version (showVersion)
import qualified Options.Applicative as Opt
import qualified Paths_elm_format as This
import qualified Text.PrettyPrint.ANSI.Leijen as PP


data Config = Config
    { _input :: FilePath
    , _output :: Maybe FilePath
    }

-- PARSE ARGUMENTS

parse :: IO Config
parse =
    Opt.customExecParser preferences parser
  where
    preferences :: Opt.ParserPrefs
    preferences =
        Opt.prefs (mempty <> Opt.showHelpOnError)

    parser :: Opt.ParserInfo Config
    parser =
        Opt.info (Opt.helper <*> flags) helpInfo


-- COMMANDS

flags :: Opt.Parser Config
flags =
    Config
      <$> input
      <*> output


-- HELP

helpInfo :: Opt.InfoMod Config
helpInfo =
    mconcat
        [ Opt.fullDesc
        , Opt.header top
        , Opt.progDesc "Format and Elm source file."
        , Opt.footerDoc (Just examples)
        ]
  where
    top =
        "elm-format " ++ showVersion This.version ++ "-alpha \n"

    examples =
        linesToDoc
        [ "Examples:"
        , "  elm-format Main.elm                     # formats Main.elm"
        , "  elm-format Main.elm --output Main2.elm  # formats Main.elm as Main2.elm"
        , ""
        , "Full guide to using elm-format at <https://github.com/avh4/elm-format>"
        ]


linesToDoc :: [String] -> PP.Doc
linesToDoc lineList =
    PP.vcat (map PP.text lineList)


yes :: Opt.Parser Bool
yes =
    Opt.switch $
        mconcat
        [ Opt.long "yes"
        , Opt.help "Reply 'yes' to all automated prompts."
        ]


dependencies :: Opt.Parser Bool
dependencies =
    Opt.switch $
        mconcat
        [ Opt.long "dependencies"
        , Opt.help "Also format this file's imported dependencies."
        ]


output :: Opt.Parser (Maybe FilePath)
output =
    Opt.optional $ Opt.strOption $
        mconcat
        [ Opt.long "output"
        , Opt.metavar "FILE"
        , Opt.help "Write output to FILE instead of overwriting the given source file."
        ]

input :: Opt.Parser FilePath
input =
    Opt.strArgument $ Opt.metavar "INPUT"
