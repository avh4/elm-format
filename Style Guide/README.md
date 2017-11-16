# Introduction

`elm-format` formats [Elm](http://elm-lang.org) source code according to a standard set of rules. It is inspired by the popular [gofmt](https://blog.golang.org/go-fmt-your-code).

The benefits of `elm-format`:
 - It makes code **easier to write**, because you never have to worry about minor formatting concerns while powering out new code.
 - It makes code **easier to read**, because there are no longer distracting minor stylistic differences between different code bases. As such, your brain can map more efficiently from source to mental model.
 - It makes code **easier to maintain**, because you can no longer have diffs related only to formatting; every diff necessarily involves a material change.
 - It **saves your team time** debating how to format things, because there is a standard tool that formats everything the same way.
 - It **saves you time** because you don't have to nitpick over formatting details of your code.


## Constraints

  - Formatting Elm code should not require any information outside of the code being formatted.  This implies:
      - the same code always formats in the same way (with a given version of `elm-format`) (**easier to read**)
      - formatting is fast because it doesn't need to do any file I/O
      - no configuration is required to help `elm-format` find other , since no other files are needed (**easier to maintain**)
  - Limit formatting choices where possible.  This helps **save you time**.
  - Avoid configuration options.  This **saves your team time**, **saves you time**, and makes code **easier to read**.
