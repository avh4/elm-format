fullyApplied f = flip f "2" 1
fullyApplied_comments f = flip{-A-}f{-B-}"2"{-C-}1
partiallyApplied f = flip f "2"
partiallyApplied_comments f = flip{-A-}f{-B-}"2"
functionOnly f = flip f
functionOnly_comments f = flip{-A-}f
unapplied = flip
extraArgs f = flip f 1 2 3 4 5

qualified f = Basics.flip f "2" 1

nested f = flip f (flip f 1 2) 3
