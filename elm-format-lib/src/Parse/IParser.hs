module Parse.IParser where

import Parse.Primitives (Parser)
import Reporting.Error.Syntax (ParsecError)


type IParser a = Parser ParsecError a
