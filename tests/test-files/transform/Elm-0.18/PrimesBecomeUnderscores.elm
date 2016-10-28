declaration' : ()
declaration' = ()

typeVariable : number -> number' -> number'' -> number'
typeVariable _ x _ = x

parameter : () -> () -> () -> ()
parameter x x' x'' x''x = ()

namedTypeConstructor : Foo' a b -> Bar'' x -> ()
namedTypeConstructor _ _ = ()

type NewType' = Foo' | Foo'' | Foo'oo

constructorReferences = (Foo', Foo'', Foo'oo)

type alias RecordType base' x' x'' = { base' | x' : (x', x''), y'' : String }

recordFields = { x' = 1, x'' = 2 }

recordUpdate = { recordFields'' | x = 1 }

recordPatterns { x', y' } = x'

patternAlias ({ x } as point') = point'
