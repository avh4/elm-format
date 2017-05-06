# Markdown

Documentation comments (`{-| -}`) contain markdown.
`elm-format` attempts to format markdown with the following goals:

* correctly format any valid Elm code blocks within the markdown
* make unintuitive behavior of the elm-markdown parser obvious to the markdown author
* avoid markdown syntax that some popular markdown implementations treat differently from elm-markdown
* as with formatting Elm code, limit formatting choices where possible


## Inline HTML

Inline HTML is left as-is.


## Block elements

Block elements are separated from one another by single blank lines.


## Paragraphs and Line Breaks

Leading and trailing whitespace is removed.

Soft breaks are maintained.
Authors are encouraged to put soft breaks between each thought
rather than reflowing the paragraph
both to aid reading comprehension
and to avoid unnecessary version control diffs.

TODO: how are hard breaks (`␠␠\n`) handled?


## Headers

Headers use the "atx-style":

```markdown
# Block Elements


## Headers
```

Headers indicate the start of a new section
and are separated from the previous block by two blank lines instead of one.


## Block Quotes

Block quotes use `>` on blank lines that are part of the block quote:

```markdown
> This is a blockquote with two paragraphs.
>
> Donec sit amet nisl.
```


## Lists

Unordered lists use hyphens (`-`) as the list marker,
indent the list marker two spaces,
and indent the item bodies 4 spaces.


```markdown
  - List item 1
  - List item 2
  - List item 3
      - Sub item
      - Sub item
```

Ordered lists use the correct numerical values,
and indent the item bodies 4 spaces.

```markdown
1.  First
2.  Second
3.  Third
```

Lists whose items contain multiple blocks
separate their items with blank lines.

```
1.  Item 1, paragraph.

    Item 2. paragraph. Long paragraph
    with soft breaks in the
    middle.

2.  Item 2.

3.  Item 3.
```
