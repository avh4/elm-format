module AllSyntaxTypes (..) where


unit : ()
unit =
    ()


lambda : () -> ()
lambda _ =
    ()


variable : a -> ()
variable _ =
    ()


tupleFn : (,,) Int String Bool -> ()
tupleFn _ =
    ()


tuple : ( a, b ) -> ()
tuple _ =
    ()


emptyRecord : {} -> ()
emptyRecord _ =
    ()


record : { x : Int, y : () } -> ()
record _ =
    ()
