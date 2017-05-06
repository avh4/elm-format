# Sections and grouping

Comments (`--`, `{- -}`) can be used to create sections within your code.


## Top-level sections

```elm
x =
    ()



-- This comment introduces a new section


a =
    ()
```


## Data structure sections

### List expressions

```elm
x =
    [ 1
    , 2

    -- This comment introduces a section in the list
    , 3
    , 4
    ]
```

### Record expressions

```elm
x =
    { x = 1
    , y = 2

    -- This comment introduces a section in the record
    , a = 3
    , b = 4
    }
```
