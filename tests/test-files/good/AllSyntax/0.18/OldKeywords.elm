module AllSyntax.OldKeywords exposing (OldKeywords, deriving, foreign, hiding)


type alias OldKeywords =
    { foreign : Int
    , hiding : Bool
    , deriving : String
    }


foreign : Float
foreign =
    0


hiding : Int -> Int
hiding x =
    x + 1


deriving : Maybe OldKeywords
deriving =
    Nothing
