module Parse.IParser where

import Control.Monad.State (State)
import qualified Parse.Primitives.Internals as I

type IParser a = I.Parser a
