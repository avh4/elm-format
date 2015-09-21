module Parse.State where

import qualified Parse.OpTable as OpTable


type State = OpTable.OpTable


init :: State
init = OpTable.empty
