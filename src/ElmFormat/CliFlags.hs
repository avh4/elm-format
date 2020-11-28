module ElmFormat.CliFlags (Config(..), parser) where

import Prelude ()
import Relude hiding (stdin)

import ElmVersion (ElmVersion(..))

import qualified ElmVersion
import qualified Data.String as String
import qualified Options.Applicative as Opt
import qualified Text.PrettyPrint.ANSI.Leijen as PP


data Config = Config
    { _input :: [FilePath]
    , _output :: Maybe FilePath
    , _yes :: Bool
    , _validate :: Bool
    , _stdin :: Bool
    , _elmVersion :: Maybe ElmVersion
    , _json :: Bool
    }


parser :: String -> Maybe String -> Opt.ParserInfo Config
parser elmFormatVersion experimental =
    Opt.info
        (Opt.helper <*> flags)
        (helpInfo elmFormatVersion experimental)



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
      <*> json


-- HELP

helpInfo :: String -> Maybe String -> Opt.InfoMod Config
helpInfo elmFormatVersion experimental =
    mconcat
        [ Opt.fullDesc
        , Opt.headerDoc $ Just top
        , Opt.progDesc "Format Elm source files."
        , Opt.footerDoc (Just examples)
        ]
  where
    top =
        PP.vcat $ concat
            [ [ PP.text $ "elm-format " ++ elmFormatVersion ]
            , case experimental of
                  Just surveyUrl ->
                      [ (PP.<$>) (PP.text "") $
                        PP.indent 4 $ PP.bold $
                        PP.fillSep $ map PP.text $ String.words $
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


elmVersion :: Opt.Parser (Maybe ElmVersion)
elmVersion =
  Opt.optional $
  Opt.option (Opt.eitherReader ElmVersion.parse) $
    mconcat
      [ Opt.long "elm-version"
      , Opt.metavar "VERSION"
      , Opt.help $
          concat
            [ "The Elm version of the source files being formatted.  "
            , "Valid values: "
            , show ElmVersion.Elm_0_18 ++ ", "
            , show ElmVersion.Elm_0_19 ++ ".  "
            , "Default: auto"
            ]
      ]


json :: Opt.Parser Bool
json =
    Opt.switch $
        mconcat
            [ Opt.long "json"
            , Opt.help "Instead of formatting, write the AST to stdout."
            , Opt.internal
            ]
