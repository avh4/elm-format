# Cheapskate

This is an experimental Markdown processor in pure Haskell.  (A cheapskate is
always in search of the best markdown.) It aims to process Markdown efficiently
and in the most forgiving possible way.  It is about seven times faster than
`pandoc` and uses a fifth the memory.  It is also faster, and considerably
more accurate, than the `markdown` package on Hackage.

There is no such thing as an invalid Markdown document. Any string of
characters is valid Markdown.  So the processor should finish efficiently no
matter what input it gets. Garbage in should not cause an error or exponential
slowdowns.  This processor has been tested on many large inputs consisting of
random strings of characters, with performance that is consistently linear with
the input size. (Try `make fuzztest`.)

## Installing

To build, get the Haskell Platform, then:

    cabal update && cabal install

This will install both the `cheapskate` executable and the Haskell
library.  A man page can be found in `man/man1` in the source.

## Usage

As an executable:

    cheapskate [FILE*]

As a library:

``` haskell
import Cheapskate
import Text.Blaze.Html

toMarkdown :: Text -> Html
toMarkdown = toHtml . markdown def
```

If the markdown input you are converting comes from an untrusted source
(e.g. a web form), you should *always* set `sanitize` to `True`.  This causes
the generated HTML to be filtered through `xss-sanitize`'s
`sanitizeBalance` function. Otherwise you risk a XSS attack from
raw HTML or a markdown link or image attribute attribute.

You may also wish to disallow users from entering raw HTML for aesthetic,
rather than security reasons.  In that case, set `allowRawHtml` to `False`,
but let `sanitize` stay `True`, since it still affects attributes coming
from markdown links and images.

## Manipulating the parsed document

You can manipulate the parsed document before rendering using the `walk`
and `walkM` functions.  For example, you might want to highlight code blocks
using highlighting-kate:

``` haskell
import Data.Text as T
import Data.Text.Lazy as TL
import Cheapskate
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text
import Text.Highlighting.Kate

markdownWithHighlighting :: Text -> Html
markdownWithHighlighting = toHtml . walk addHighlighting . markdown def

addHighlighting :: Block -> Block
addHighlighting (CodeBlock (CodeAttr lang _) t) =
  HtmlBlock (T.concat $ TL.toChunks
             $ renderHtml $ toHtml
             $ formatHtmlBlock defaultFormatOpts
             $ highlightAs (T.unpack lang) (T.unpack t))
addHighlighting x = x
```

## Extensions

This processor adds the following Markdown extensions:

### Hyperlinked URLs

All absolute URLs are automatically made into hyperlinks, where
inside `<>` or not.

### Fenced code blocks

Fenced code blocks with attributes are allowed.  These begin with
a line of three or more backticks or tildes, followed by an
optional language name and possibly other metadata.  They end
with a line of backticks or tildes (the same character as started
the code block) of at least the length of the starting line.

### Explicit hard line breaks

A hard line break can be indicated with a backslash before a
newline. The standard method of two spaces before a newline also
works, but this gives a more "visible" alternative.

### Backslash escapes

All ASCII symbols and punctuation marks can be backslash-escaped,
not just those with a use in Markdown.

## Revisions

In departs from the markdown syntax document in the following ways:

### Intraword emphasis

Underscores cannot be used for word-internal emphasis. This
prevents common mistakes with filenames, usernames, and indentifiers.
Asterisks can still be used if word in*ter*nal emphasis is needed.

The exact rule is this:  an underscore that appears directly after
an alphanumeric character does not begin an emphasized span.  (However,
an underscore directly before an alphanumeric can end an emphasized
span.)

### Ordered lists

The starting number of an ordered list is now significant.
Other numbers are ignored, so you can still use `1.` for each
list item.

In addition to the `1.` form, you can use `1)` in your ordered lists.
A new list starts if you change the form of the delimiter. So, the
following is two lists:

    1. one
    2. two
    1) one
    2) two

### Bullet lists

A new bullet lists starts if you change the bullet marker.
So, the following is two consecutive bullet lists:

    + one
    + two
    - one
    - two

### List separation

Two blank lines breaks out of a list.  This allows you to
have consecutive lists:

    - one

    - two


    - one (new list)

The blank lines break out of a list no matter how deeply it
is nested:

    - one
      - two
        - three


      - new top-level list

### Indentation of list continuations

Block elements inside list items need not be indented four
spaces.  If they are indented beyond the bullet or numerical
list marker, they will be considered additional blocks inside
the list item.  So, the following is a list item with two paragraphs:

    - one

     two

The amount of indentation required for an indented code block
inside a list item depends on the first line of the list item.
Generally speaking, code must be indented four spaces past the
first non-space character after the list marker.  Thus:

     -   My code

             {code here}

     - My code

           {code here}

The following diagram shows how the first line of a list item
divides the following lines into three regions:

     -   My code
      |     |
      +-----+

Content to the left of the marked region will not be part of the list
item.  Content to the right of the marked region will be indented code
under the list item.  Regular blocks that belong under the
list item should start inside the marked region.

When the first line itself contains indented code, this code
and subsequent indented code blocks should be indented five spaces past the
list marker:

     -     { code }

           { more code }

### Raw HTML blocks

Raw HTML blocks work a bit differently than in `Markdown.pl`.
A raw HTML block starts with a block-level HTML tag (opening or
closing), or a comment start `<!--` or end `-->`, and goes until
the next blank line.  The whole block is included as raw HTML.
No attempt is made to parse balanced tags.  This means that
in the following, the asterisks are literal asterisks:

    <div>
    *hello*
    </div>

while in the following, the asterisks are interpreted as markdown
emphasis:

    <div>

    *hello*

    </div>

In the first example, we have a single raw HTML block; in the second,
we have two raw HTML blocks with an intervening paragraph.  This system
provides flexibility to authors to use enclose markdown sections
in html block-level tags if they wish, while also allowing them
to include verbatim HTML blocks (taking care that the don't include
any blank lines).

As a consequence of this rule, HTML blocks may not contain blank lines.

## Clarifications

This implementation resolves the following issues left vague in the markdown
syntax document:

### Tight vs. loose lists

A list is considered "tight" if (a) it has only one item or
there is no blank space between any two consecutive items, and
(b) no item has blank lines as its immediate children.
If a list is "tight," then list items consisting of a single
paragraph or a paragraph followed by a sublist will be rendered
without `<p>` tags.

### Sublists

Sublists work like other block elements inside list items;
they  must be indented past the bullet or numerical list marker
(but no more than three spaces past, or they will be interpreted
as indented code).

### ATX headers

ATX headers must have a space after the initial `###`s.

### Separation of block quotes

A blank line will end a blockquote. So, the following is a single
blockquote:

    > hi
    >
    > there

But this is two blockquotes:

    > hi

    > there

Blank lines are not required before horizontal rules, blockquotes,
lists, code blocks, or headers.  They are not required after, either,
though in many cases "laziness" will effectively require a blank
line after.  For example, in

    Hello there.
    > A quote.
    Still a quote.

the "Still a quote." is part of the block quote, because of laziness
(the ability to leave off the > from the beginning of subsequent
lines).  Laziness also affects lists. However, we can have a code
block, ATX header, or horizontal rule between two paragraphs without any
blank lines.

### Link references

Link references may occur anywhere in the document, even in nested
list contexts.  They need not be at the outer level.

## Tests

The `tests` subdirectory contains an extensive suite of tests,
including all of John Gruber's original Markdown tests, plus
many of the tests from Michel Fortin's `mdtest` suite.  Each
test consists in two files with the same basename, a markdown
source and an expected HTML output.

To run the test suite, do

    make test

To run only tests that match a regex pattern, do

    PATT=Orig make test

Setting the environment variable `TIDY=1` will run the expected and
actual output through tidy before comparing them.  You can run this
test suite on another markdown processor by doing

    PROG=myothermarkdown make test

## Benchmarks

To run a crude benchmark comparing `cheapskate` to `pandoc`, do
`make bench`.  Set the `BENCHPROGS` environment variable to
compare to other implementations.

## License

Copyright &copy; 2012, 2013, 2014 John MacFarlane.

The library is released under the BSD license; see LICENSE for terms.

Some of the test cases are borrowed from Michel Fortin's mdtest suite
and John Gruber's original markdown test suite.
