---
title: cheapskate
section: 1
footer: cheapskate manual
date: January 4, 2014
...

# NAME

cheapskate - convert markdown to HTML.

# SYNOPSIS

cheapskate [*options..*] [*file..*]

# DESCRIPTION

`cheapskate` will convert the input text (which may come from stdin
or from any number of input files) from markdown to HTML.  Output
will be written to stdout.

# OPTIONS

`-s, --sanitize`
:   Sanitize raw HTML and link and image attributes to protect from
    XSS attacks.

`-e, --escape-raw-html`
:   Escape all raw HTML.  (Note:  for safety, use `--sanitize`. This
    option is for those who want to disallow raw HTML to enforce
    uniformity of formatting.)

`-b, --hard-line-breaks`
:   Treat all newlines as hard line breaks (`<br />` tags in HTML).

`--debug`
:   Print the container structure obtained from phase 1 of parsing.
    For debugging only.

`-h, --help`
:   Print usage information.

`-V, --version`
:   Print version.

# AUTHORS

John MacFarlane.

# SEE ALSO

The `cheapskate` source code and all documentation may be downloaded
from <http://github.com/jgm/cheapskate/>.
