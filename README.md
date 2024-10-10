Configuration Is Code Blog
==========================

Copyright 2018-2019 by Curt J. Sampson. All rights reserved.

This blog uses, among other things: [Hakyll]; [Bootstrap 4].

### Building

Run `./Test` to build the site builder, run the tests, and run the
site builder to build the site. (You may also use `stack` directly,
but note that you should be including any command-line options that
`Test` passes to `stack`.)


To-do List
----------

* Add Disqus comments.
* What should the `/blog/` page do? Redirect to `/`, now that `/` is
  the main listing page?
* The site could be made a lot prettier.
* [Pug] has replaced [Jade]; we should change to it.
* `npm` mentions that several other packages we're using are
  deprecated versions; we should update.
* Should we be using Bootstrap from its CDN? Probably not, because
  we also want to be able to develop when offline.



<!-------------------------------------------------------------------->
[Hakyll]: https://jaspervdj.be/hakyll/
[Bootstrap 4]: http://v4-alpha.getbootstrap.com/
