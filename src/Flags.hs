{-# OPTIONS_GHC -Wall #-}
module Flags where

import Data.Monoid ((<>))
import Data.Version (showVersion)
import Options.Applicative ((<|>))

import qualified Options.Applicative as Opt
import qualified Paths_elm_format as This
import qualified Text.PrettyPrint.ANSI.Leijen as PP


data Config = Config
    { _input :: Maybe [FilePath]
    , _output :: Maybe FilePath
    , _yes :: Bool
    }

-- PARSE ARGUMENTS

parse :: IO Config
parse =
    Opt.customExecParser preferences parser

preferences :: Opt.ParserPrefs
preferences =
    Opt.prefs (mempty <> Opt.showHelpOnError)

parser :: Opt.ParserInfo Config
parser =
    Opt.info (Opt.helper <*> flags) helpInfo

showHelpText :: IO ()
showHelpText = Opt.handleParseResult . Opt.Failure $
  Opt.parserFailure preferences parser Opt.ShowHelpText mempty



-- COMMANDS

flags :: Opt.Parser Config
flags =
    Config
      <$> someInput input stdin
      <*> output
      <*> yes


-- HELP

helpInfo :: Opt.InfoMod Config
helpInfo =
    mconcat
        [ Opt.fullDesc
        , Opt.header top
        , Opt.progDesc "Format an Elm source file."
        , Opt.footerDoc (Just examples)
        ]
  where
    top =
        "elm-format " ++ showVersion This.version ++ " \n"

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

-- joins the use of
someInput
    :: Opt.Mod Opt.ArgumentFields FilePath
    -> Opt.Mod Opt.FlagFields Bool
    -> Opt.Parser (Maybe [FilePath])
someInput argumentFields flagFields =
    Just <$> Opt.some argInput <|> stdinSwitch
        where
            -- if there's a switch value of true, then
            -- return the stdin path
            -- otherwise Nothing
            stdinSwitch :: Opt.Parser (Maybe [FilePath])
            stdinSwitch =
                switchToMaybe <$> Opt.switch flagFields

            argInput :: Opt.Parser FilePath
            argInput =
                Opt.strArgument argumentFields

            switchToMaybe xs =
                case xs of
                    True -> Just ["-"]
                    False -> Nothing


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

input :: Opt.Mod Opt.ArgumentFields FilePath
input =
    Opt.metavar "INPUT"

stdin :: Opt.Mod Opt.FlagFields Bool
stdin =
    mconcat
    [ Opt.long "stdin"
    , Opt.help "Read from stdin, output to stdout."
    ]
