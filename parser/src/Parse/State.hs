module Parse.State where

import qualified Parse.OpTable as OpTable


data State = State
	{ ops :: OpTable.OpTable
	}


init :: State
init =
	State
		{ ops = OpTable.empty
		}


setOps :: OpTable.OpTable -> State -> State
setOps table _ =
	State
		{ ops = table
		}