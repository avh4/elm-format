module Parse.State where


data State = State
  { newline :: [Bool]
  }


init :: State
init = State { newline = [False] }


setNewline :: State -> State
setNewline state = case newline state of
  []         -> state
  (_ : rest) -> state { newline = (True : rest) }


pushNewlineContext :: State -> State
pushNewlineContext state = state { newline = (False : (newline state)) }


popNewlineContext :: State -> State
popNewlineContext state = case newline state of
  []                   -> state
  (_           : []  ) -> state
  (last : next : rest) -> state { newline = (last || next) : rest }


sawNewline :: State -> Bool
sawNewline state = case newline state of
  []      -> False
  (b : _) -> b
