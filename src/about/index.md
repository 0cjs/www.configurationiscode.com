---
title: Configuration Is Code!
template: page.jade
---

Configuration Is Code
=====================

Many developers seem to think that the text in configuration files,
such as for a web server, is something separate from the code (e.g.,
Ruby code) for their application. This is not true: anything you put
in a file that determines how your application runs (and can break
your application if it's incorrect) is not "like" code or "similar to"
code; it _is_ part of the application's code.

This implies that it should be treated as you treat all your other
code: it should be in your version control repository for the
application, it should be covered by functional and unit (where
applicable) tests, and so on.

The [blog](/blog) here discusses this and various other issues related
to integrating your development, testing, release and operations
processes, including:

* Where configuration should be stored
* Building configuration files from configuration "source code"
* How to test configuration, especially _before_ it's rolled out
  in to production or even staging

All of this falls under the rubric of what is often called [DevOps],
which I consider to be a much-needed removal of the separation between
software development and IT operations, just as agile development has
removed much of the separation between coding and QA. But be mindful
that many people seem to use this term in a different way, as a
synonym for IT operations, sometimes going so far as to have a
"DevOps" group completely separate from their developers.

[DevOps]: https://en.wikipedia.org/wiki/DevOps
