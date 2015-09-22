module Parse.State where

import qualified Parse.OpTable as OpTable


data State = State
	{ ops :: OpTable.OpTable
	, comments :: [String]
	}


init :: State
init =
	State
		{ ops = OpTable.empty
		, comments = []
		}


setOps :: OpTable.OpTable -> State -> State
setOps table state =
	state { ops = table }


addComment :: String -> State -> State
addComment comment state =
	state { comments = comment : (comments state) }


clearComments :: State -> State
clearComments state =
	state { comments = [] }
