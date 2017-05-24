module OurSpec exposing (spec)


spec : Test
spec =
    describe "OurSpec" [ aSpec, bSpec ]


aSpec : Test
aSpec =
    describe "A" []
