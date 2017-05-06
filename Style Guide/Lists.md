# Lists


## Empty lists

An empty list is formatted as:

```elm
[]
```

You should avoid putting comments inside of an empty list.
Instead, put explanatory comments before the empty list.

```bad-elm
[ {- don't put comments here -} ]
```

```elm
foldl step
  -- accumulator
  []
  items
```


## Small lists

Small lists are kept on a single line:

```elm
[1, 2, 3]
```


## Large lists

Large lists put one item on each line.
Commas are put at the beginning of each new line.
You can separate sections within the list using comments.

```elm
[ 1
, 2

-- new section
, 3
, 4
]
```
