---
title: Configuration Is Code
publishDate: 2017-09-05
modifyDate: 2017-09-05
author: Curt J. Sampson
email: cjs@cynic.net
template: post.jade
---

The [about page](/about) of this site briefly tells you that
"configuration is code"; this post goes into some of the the details.

Back in the not-so-good old days of plain old Apache and PHP sites the
configuration file for Apache was almost invariably separate from the
site itself, usually stored in `/etc/` on the production server and
and its backup tapes and nowhere else. This persists even today: the
default for web server packages in most Linux distributions has the
configuration in `/etc/` but the data being read out of `/var/www`,
perhaps users' home directories, and who knows where else.

The configuration stored in these files can be divided into two basic
parts.

One part is configuration related to the particular installation of
the web server itself, such as the address and port on which to
listen, connection limits and other performance settings, location of
TLS key and certificate files, the locations of applications on this
server, and so on. While this is essential for the web application to
run as it's supposed to, it can be considered configuration for this
particular instance of the application: it may change for an instance
where, for example, you don't need HTTPS security (such as with a local
development instance).

The second part is what I call "application configuration." And when
I use the word "application" here I'm not talking just about PHP,
Ruby or similar code
executing on a server, accessing databases, and so on. I'm talking
about all the content being served, even if it's just static HTML, CSS
and JS files.[^1] Even a static website relies on a surprising amount
of web server configuration to work properly.

A very common example is URL rewriting and redirects. We're all aware
of static sties that use redirects for, say, letting old links
work when a site reorganizes the paths it uses. An Apache configuration
fragment to do this might look like:

    RewriteEngine On
    RewriteRule ^contact/(.*)$ %{HTTP_HOST}/about/contact/$1 [R=301,L]

(I simplify here; there's actually a lot more configuration necessary
to ensure that this fragment works. Further, this is specific to the
particular webserver being used; NGINX would fail with the above code,
requiring instead something along the lines of
`rewrite ^/contact/$ /$http_host/about/contact/ permanent;`.
The existence of these complexities is part of the point of this post.)

But the rewriting used by a static site is often more subtle and hidden. It's
common for people to use links such as `/about` expecting that the
page that will be served will come from `about/index.html` in the
filesystem; this relies on two different configuration rules:

1. The `/about` path in an HTTP request will produce a redirect to a
   slightly different path, `/about/`, because the web server is
   configured to redirect to a new path with a trailing slash
   when the original path refers to a directory under the document root.

2. The `/about/` path quietly serves not the contents of the directory
   inode in the filesystem or a directory listing, but the contents of
   `about/index.html` in that directory.

This isn't even the end of the configuration required to serve this simple
page: there's yet more going on here.
A web server returns a `Content-type` header with every response and
correct setting of this is required for the browser to properly render
the content. The content of this header usually isn't stored in the file
being served; instead web servers are typically configured to generate it
based on the file extension. Here's some example configuration
extracted `/etc/nginx/mime.types` on a Debian 9 system:

    types {
        text/html                             html htm shtml;
        text/css                              css;
        image/gif                             gif;
        image/jpeg                            jpeg jpg;
        application/javascript                js;
        text/plain                            txt;
    }

Without this configuration in place the web server might return
`Content-type: text/plain` for an HTML file, thus causing the browser
to show the HTML source code to the user. For most sites that would
clearly be broken, thus this configuration is an essential part of the
site code.



XXX


[^1]: In fact, interactive applications that are completely "static" from
the web server's point of view are becoming more and more common these
days. These are called [JAMstack] applications and are popular because
they can be served very quickly from a [CDN].

[JAMstack]: https://jamstack.org/
[CDN]: https://en.wikipedia.org/wiki/Content_delivery_network


##### XXX To-do:

* footnote rendering
* tweak CSS to indent code
* `Options FollowSymLinks`
* Check  https://mediatemple.net/community/products/dv/204643270/using-htaccess-rewrite-rules for other interesting stuff
* https://www.thesitewizard.com/apache/redirect-domain-www-subdomain.shtml
* .htaccess
* conf.d/
* php.conf
* mention site code in databases (à la WordPress)
* Nice example of how hairy configuration can get: https://www.digitalocean.com/community/questions/configuring-multiple-domains-and-subdomains-dns-nginx-issues-abound
* `.htaccess` efficiency and why NGINX doesn't like it: https://www.nginx.com/resources/wiki/start/topics/examples/likeapache-htaccess/
