{-# OPTIONS_GHC -Wall #-}
module Flags where

import Data.Monoid ((<>))
import Data.Version (showVersion)
import ElmVersion (ElmVersion(..))

import qualified Data.Maybe as Maybe
import qualified ElmVersion
import qualified Options.Applicative as Opt
import qualified Paths_elm_format as This
import qualified Text.PrettyPrint.ANSI.Leijen as PP


data Config = Config
    { _input :: [FilePath]
    , _output :: Maybe FilePath
    , _yes :: Bool
    , _validate :: Bool
    , _stdin :: Bool
    , _elmVersion :: ElmVersion
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
      <$> Opt.many input
      <*> output
      <*> yes
      <*> validate
      <*> stdin
      <*> elmVersion



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

yes :: Opt.Parser Bool
yes =
    Opt.switch $
        mconcat
        [ Opt.long "yes"
        , Opt.help "Reply 'yes' to all automated prompts."
        ]

validate :: Opt.Parser Bool
validate =
    Opt.switch $
        mconcat
        [ Opt.long "validate"
        , Opt.help "Check if files are formatted without changing them."
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

stdin :: Opt.Parser Bool
stdin =
    Opt.switch $
        mconcat
        [ Opt.long "stdin"
        , Opt.help "Read from stdin, output to stdout."
        ]


elmVersion :: Opt.Parser ElmVersion
elmVersion =
  fmap (Maybe.fromMaybe Elm_0_16)$
  Opt.optional $
  Opt.option (Opt.eitherReader ElmVersion.parse) $
    mconcat
      [ Opt.long "elm-version"
      , Opt.metavar "VERSION"
      , Opt.help "The Elm version of the source files being formatted.  Valid values: 0.16, 0.17.  Default: 0.16"
      ]
