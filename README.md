configurationiscode.com
-----------------------

This branch contains the web site for `www.configurationiscode.com`
and any other domain names that should simply redirect to
`www.configurationiscode.tech`. The `_redirects` file configures
`netlify.com` to send HTTP 301 redirects for any reqeust to this
"site," preserving any path on the URL.

Note that any files in the site will still be served; the redirect code
is not used when the URL does matches an exact filename.
