module Parse.IParser where

import Parse.Primitives (Parser)
import Parse.ParsecAdapter (ParseError)


type IParser a = Parser ParseError a
