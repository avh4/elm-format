module Parse.State where


data State = State
  { comments :: [String]
  , newline :: [Bool]
  }


init :: State
init =
  State
    { comments = []
    , newline = []
    }


addComment :: String -> State -> State
addComment comment state =
    state { comments = comment : (comments state) }


clearComments :: State -> State
clearComments state =
    state { comments = [] }


setNewline :: State -> State
setNewline state =
    case newline state of
        [] -> state
        (_:rest) -> state { newline = (True:rest) }


pushNewlineContext :: State -> State
pushNewlineContext state =
    state { newline = (False:(newline state)) }


popNewlineContext :: State -> State
popNewlineContext state =
    case newline state of
        [] -> state
        (_:rest) -> state { newline = rest }


sawNewline :: State -> Bool
sawNewline state =
    case newline state of
        [] -> False
        (b:_) -> b
