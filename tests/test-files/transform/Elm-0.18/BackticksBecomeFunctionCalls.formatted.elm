module Main exposing (needParens, nested, simple, withComments, withComments2, withLabmdas, withSymbolOps)


simple =
    f 1 2


nested =
    h (g (f 1 2) 3) 4


withSymbolOps =
    1 + f 2 3 + g 4 5 + 6


withComments =
    g {- C -} (f {- A -} 1 {- B -} 2) {- D -} 3


needParens =
    def (abc (f x y) (g x y)) (h x y)


withComments2 =
    abc {- A -} (f x y) {- B -} (g x y)


withLabmdas =
    qq a
        (\x ->
            ww (f x) (\y -> g x y)
        )
