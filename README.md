Configuration Is Code Blog
==========================

Copyright 2018-2019 by Curt J. Sampson. All rights reserved.

This blog uses, among other things: [Hakyll]; [Bootstrap 4].

### Building

Run `./Test` to build the code and the site. This will:
- Build `site-compiler` and `test`;
- Run `test` to unit test Haskell code; and
- Run `site-compiler rebuild` to do a clean build of the website.

`Test` accepts some additional options; use `-h` to see help.

If you supply additional command line arguments to `Test`, these will be
passed to `site-compiler`, replacing the `rebuild` command that's normally
run. A common one is `./Test watch` to start a preview server and rebuild
when site files are changed. Use `./Test -- -h` for site-compiler help.

You may also use `stack` directly to run arbitrary commands, but make sure
to include any command-line options that `Test` passes to `stack`.


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
