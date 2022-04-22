module Parse.IParser where

import Parse.Primitives (Parser)
import Reporting.Error.Syntax (ParsecError)
import AST.V0_16 (UppercaseIdentifier)
import qualified Data.Indexed as I
import Reporting.Annotation (Located)
import AST.Structure (ASTNS)


type IParser a = Parser ParsecError a


type ParsedAST = I.Fix2 Located (ASTNS [UppercaseIdentifier])
