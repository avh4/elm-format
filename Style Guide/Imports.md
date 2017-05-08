# Imports

Imports are sorted alphabetically by the qualified name of the module being imported.

> **Why sort?**
> Sorting imports makes code [easier to maintain][intro] because it reduces and simplifies
> merge conflicts when multiple changes are made to the imports in a module.
> Sorting imports also [saves you time][intro] because you won't spend time thinking
> about how to order your imports.

> **Why sort by qualified name?**
> The qualified name is the most visually-obvious part of the import due to
> its consistent location on each import line.
> Sorting by qualified name makes it the easiest to find the import
> you are looking for when visually scanning the imports.

Exposed values in an import are sorted in the following way:

1.  symbolic operators (in ASCII order)
2.  data types (in alphabetical order)
3.  other values (in alphabetical order)

> **Why this order?**
> The ordering of the `exposing` clause is meant to highlight the information
> that is most useful when getting familiar with a module.
> Symbolic operators are rare and are the most important to notice because
> their meaning is often obscure and difficult to search for.
> Data types are next because it is common to try to get an understanding of
> the relevant data before looking at the details of the implementation.

> **Why alphabetical order?**
> Another reasonable ordering would be the order that the imported values are
> defined in the documentation of the imported module.
> However, sorting in that way would require knowledge of the imported module,
> and cannot be done under the [desired constraints](./#constraints).

If a data type exposes tags, they are sorted in alphabetical order.


[intro]: ./#introduction
