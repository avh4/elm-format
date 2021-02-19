module ElmFormat.AST.PublicAST.Config (Config(..)) where


newtype Config =
    Config
        { showSourceLocation :: Bool
        }
