module Parse.State where


data State = State
  { comments :: [String]
  , newline :: Bool
  }


init :: State
init =
  State
    { comments = []
    , newline = False
    }


addComment :: String -> State -> State
addComment comment state =
    state { comments = comment : (comments state) }


clearComments :: State -> State
clearComments state =
    state { comments = [] }


setNewline :: State -> State
setNewline state =
    state { newline = True }


clearNewline :: State -> State
clearNewline state =
    state { newline = False }
