module Fifo exposing
    ( Fifo, empty, fromList
    , insert, remove
    , toList
    )

{-|


# Creating FIFOs

@docs Fifo, empty, fromList


# Inserting/Removing

@docs insert, remove


# To List

@docs toList

-}


{-| A FIFO containing items of type `a`.
-}
type Fifo a
    = Fifo (List a) (List a)


{-| Creates an empty Fifo.

    Fifo.empty
        -- == Fifo.fromList []

-}
empty : Fifo a
empty =
    Fifo [] []


{-| Inserts an item into a Fifo

    Fifo.empty
    |> Fifo.insert 7
    |> Fifo.insert 8
        -- == Fifo.fromList [7,8]

-}
insert : a -> Fifo a -> Fifo a
insert a (Fifo front back) =
    Fifo front (a :: back)


{-| Removes the next (oldest) item from a Fifo, returning the item (if any), and the updated Fifo.

    Fifo.fromList [3,7]
    |> Fifo.remove
        -- == (Just 3, Fifo.fromList [7])

-}
remove : Fifo a -> ( Maybe a, Fifo a )
remove fifo =
    case fifo of
        Fifo [] [] ->
            ( Nothing, empty )

        Fifo [] back ->
            remove <| Fifo (List.reverse back) []

        Fifo (next :: rest) back ->
            ( Just next, Fifo rest back )


{-| Creates a Fifo from a List.

    Fifo.fromList [3,4,5]
    |> Fifo.remove
    |> fst
        -- == Just 3

-}
fromList : List a -> Fifo a
fromList list =
    Fifo list []


{-| Converts a Fifo to a List.

    Fifo.empty
    |> Fifo.insert 7
    |> Fifo.insert 9
    |> Fifo.toList
        -- == [7,9]

-}
toList : Fifo a -> List a
toList (Fifo front back) =
    front ++ List.reverse back
