type Foo
    = A
    | B
    | C

a = A

b = case a then
        A -> 1
        B -> 2
        C -> 3
