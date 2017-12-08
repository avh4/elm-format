module Flags where

import Data.Monoid ((<>))
import ElmVersion (ElmVersion(..))

import qualified Data.Maybe as Maybe
import qualified ElmVersion
import qualified Options.Applicative as Opt
import qualified Text.PrettyPrint.ANSI.Leijen as PP


data Config = Config
    { _input :: [FilePath]
    , _output :: Maybe FilePath
    , _yes :: Bool
    , _validate :: Bool
    , _stdin :: Bool
    , _elmVersion :: ElmVersion
    , _upgrade :: Bool
    }



-- PARSE ARGUMENTS


parse :: ElmVersion -> String -> Maybe String -> [String] -> Opt.ParserResult Config
parse defaultElmVersion elmFormatVersion experimental args =
    Opt.execParserPure preferences (parser defaultElmVersion elmFormatVersion experimental) args


usage :: ElmVersion -> String -> String -> Maybe String -> String
usage defaultVersion progName version experimental =
    fst $
    Opt.renderFailure
        (Opt.parserFailure preferences (parser defaultVersion version experimental) Opt.ShowHelpText mempty)
        progName


preferences :: Opt.ParserPrefs
preferences =
    Opt.prefs (mempty <> Opt.showHelpOnError)


parser :: ElmVersion -> String -> Maybe String -> Opt.ParserInfo Config
parser defaultElmVersion elmFormatVersion experimental =
    Opt.info
        (Opt.helper <*> flags defaultElmVersion)
        (helpInfo defaultElmVersion elmFormatVersion experimental)


showHelpText :: ElmVersion -> String -> Maybe String -> Opt.ParserResult never
showHelpText defaultElmVersion elmFormatVersion experimental = Opt.Failure $
    Opt.parserFailure
        preferences
        (parser defaultElmVersion elmFormatVersion experimental)
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
      <*> upgrade



-- HELP

helpInfo :: ElmVersion -> String -> Maybe String -> Opt.InfoMod Config
helpInfo defaultElmVersion elmFormatVersion experimental =
    mconcat
        [ Opt.fullDesc
        , Opt.headerDoc $ Just top
        , Opt.progDesc "Format Elm source files."
        , Opt.footerDoc (Just examples)
        ]
  where
    top =
        PP.vcat $ concat
            [ [ PP.text $ "elm-format-" ++ show defaultElmVersion ++ " " ++ elmFormatVersion ]
            , case experimental of
                  Just surveyUrl ->
                      [ (PP.<$>) (PP.text "") $
                        PP.indent 4 $ PP.bold $
                        PP.fillSep $ map PP.text $ words $
                          "This version of elm-format contains features " ++
                          "that may or may not appear in future releases. " ++
                          "You can provide feedback about experimental features " ++
                          "at " ++ surveyUrl
                      ]
                  Nothing ->
                      []
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
            , show ElmVersion.Elm_0_17 ++ ", "
            , show ElmVersion.Elm_0_18 ++ ".  "
            , "Default: " ++ show defaultVersion
            ]
      ]


upgrade :: Opt.Parser Bool
upgrade =
    Opt.switch $
        mconcat
        [ Opt.long "upgrade"
        , Opt.help "Upgrade older Elm files to Elm 0.18 syntax"
        ]
