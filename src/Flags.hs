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


parse :: ElmVersion -> IO Config
parse defaultVersion =
    Opt.customExecParser preferences (parser defaultVersion)


parse' :: ElmVersion -> [String] -> Either (Opt.ParserResult never) Config
parse' defaultVersion args =
    case Opt.execParserPure preferences (parser defaultVersion) args of
        Opt.Success config ->
            Right config

        Opt.Failure failure ->
            Left $ Opt.Failure failure

        Opt.CompletionInvoked completion ->
            Left $ Opt.CompletionInvoked completion


usage :: ElmVersion -> String -> String
usage defaultVersion progName =
    fst $
    Opt.renderFailure
        (Opt.parserFailure preferences (parser defaultVersion) Opt.ShowHelpText mempty)
        progName


preferences :: Opt.ParserPrefs
preferences =
    Opt.prefs (mempty <> Opt.showHelpOnError)


parser :: ElmVersion -> Opt.ParserInfo Config
parser defaultVersion =
    Opt.info
        (Opt.helper <*> flags defaultVersion)
        (helpInfo defaultVersion)


showHelpText :: ElmVersion -> IO ()
showHelpText defaultVersion = Opt.handleParseResult . Opt.Failure $
    Opt.parserFailure
        preferences
        (parser defaultVersion)
        Opt.ShowHelpText
        mempty



-- COMMANDS

flags :: ElmVersion -> Opt.Parser Config
flags defaultVersion =
    Config
      <$> Opt.many input
      <*> output
      <*> yes
      <*> validate
      <*> stdin
      <*> elmVersion defaultVersion



-- HELP

helpInfo :: ElmVersion -> Opt.InfoMod Config
helpInfo defaultVersion =
    mconcat
        [ Opt.fullDesc
        , Opt.header top
        , Opt.progDesc "Format Elm source files."
        , Opt.footerDoc (Just examples)
        ]
  where
    top =
        concat
            [ "elm-format-" ++ show defaultVersion ++ " "
            , showVersion This.version  ++ "-alpha-dev-20161030" ++ "\n"
            ]

    examples =
        linesToDoc
        [ "Examples:"
        , "  elm-format Main.elm                     # formats Main.elm"
        , "  elm-format Main.elm --output Main2.elm  # formats Main.elm as Main2.elm"
        , "  elm-format src/                         # format all *.elm files in the src directory"
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


elmVersion :: ElmVersion -> Opt.Parser ElmVersion
elmVersion defaultVersion =
  fmap (Maybe.fromMaybe defaultVersion)$
  Opt.optional $
  Opt.option (Opt.eitherReader ElmVersion.parse) $
    mconcat
      [ Opt.long "elm-version"
      , Opt.metavar "VERSION"
      , Opt.help $
          concat
            [ "The Elm version of the source files being formatted.  "
            , "Valid values: "
            , show ElmVersion.Elm_0_16 ++ ", "
            , show ElmVersion.Elm_0_17 ++ ", "
            , show ElmVersion.Elm_0_18 ++ ".  "
            , "Default: " ++ show defaultVersion
            ]
      ]
