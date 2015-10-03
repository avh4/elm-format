module Parse.State where


data State = State
  { comments :: [String]
  }


init :: State
init =
  State
    { comments = []
    }


addComment :: String -> State -> State
addComment comment state =
  state { comments = comment : (comments state) }


clearComments :: State -> State
clearComments state =
  state { comments = [] }
