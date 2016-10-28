module Main exposing (..)


declaration_ : ()
declaration_ =
    ()


typeVariable : number -> number_ -> number__ -> number_
typeVariable _ x _ =
    x


parameter : () -> () -> () -> ()
parameter x x_ x__ x__x =
    ()


namedTypeConstructor : Foo_ a b -> Bar__ x -> ()
namedTypeConstructor _ _ =
    ()


type NewType_
    = Foo_
    | Foo__
    | Foo_oo


constructorReferences =
    ( Foo_, Foo__, Foo_oo )


type alias RecordType base_ x_ x__ =
    { base_ | x_ : ( x_, x__ ), y__ : String }


recordFields =
    { x_ = 1, x__ = 2 }


recordUpdate =
    { recordFields__ | x = 1 }


recordPatterns { x_, y_ } =
    x_


patternAlias ({ x } as point_) =
    point_
