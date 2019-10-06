module ElmRefactor.CliFlags (Flags(..), parser) where

import Prelude ()
import Relude hiding (stdin)

import qualified Options.Applicative as Opt
import qualified Text.PrettyPrint.ANSI.Leijen as PP


data Flags = Flags
    { _upgradeDefinition :: FilePath
    , _input :: [FilePath]
    }


parser :: String -> Opt.ParserInfo Flags
parser elmRefactorVersion =
    Opt.info
        (Opt.helper <*> flags)
        (helpInfo elmRefactorVersion)



-- COMMANDS

flags :: Opt.Parser Flags
flags =
    Flags
      <$> upgradeDefinition
      <*> Opt.many input


-- HELP

helpInfo :: String -> Opt.InfoMod Flags
helpInfo elmRefactorVersion =
    mconcat
        [ Opt.fullDesc
        , Opt.headerDoc $ Just top
        , Opt.progDesc "Refactor Elm source files."
        , Opt.footerDoc (Just examples)
        ]
  where
    top =
        PP.vcat $ concat
            [ [ PP.text $ "elm-refactor " ++ elmRefactorVersion ]
            ]

    examples =
        linesToDoc
        [ "Examples:"
        , "  elm-refactor Upgrade.elm Main.elm      # refactors Main.elm"
        , "  elm-refactor Upgrade.elm src/          # refactors all *.elm files in the src directory"
        -- , ""
        -- , "Full guide to using elm-refactor at <https://github.com/avh4/elm-format>"
        ]


linesToDoc :: [String] -> PP.Doc
linesToDoc lineList =
    PP.vcat (map PP.text lineList)


upgradeDefinition :: Opt.Parser FilePath
upgradeDefinition =
    Opt.strArgument $ Opt.metavar "UPGRADE_DEFINITION"


input :: Opt.Parser FilePath
input =
    Opt.strArgument $ Opt.metavar "INPUT"
