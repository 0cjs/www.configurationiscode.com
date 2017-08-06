'use strict'

const devMode = false   // If true, run a local server

const metalsmith = require('metalsmith'),
    branch = require('metalsmith-branch'),
    collections = require('metalsmith-collections'),
    excerpts = require('metalsmith-excerpts'),
    markdown = require('metalsmith-markdown'),
    permalinks = require('metalsmith-permalinks'),
    serve = require('metalsmith-serve'),
    templates = require('metalsmith-templates'),
    watch = require('metalsmith-watch'),
    moment = require('moment');

// `branch` and `collections` use multimatch patterns:
//     https://github.com/sindresorhus/multimatch
// The original tutorial misinterpreted the documentation as saying
// the `**` in `posts/**.html` would match a path with slashes in it,
// but that is actually true only when `**` is "the only thing in a
// path part," which appears to mean when it's `/**/`.

const blogEntries = 'blog/**/*.html'

const pipeline = metalsmith(__dirname)
    .metadata({
        site: {
        title: 'configurationiscode.com',
        url: 'http://configurationiscode.com'
    }})
    .source('./src')
    .destination('./build')
    .use(markdown())
    .use(excerpts())
    .use(collections({
        posts: {
            pattern: [blogEntries, '!blog/index.html'],
            sortBy: 'publishDate',
            reverse: true
        }
    }))
    .use(branch(blogEntries)
        .use(permalinks({
            pattern: 'blog/:publishDate/:title/',
            date: 'YYYY/MM',
            relative: false
        }))
    )
    .use(branch('!' + blogEntries)
        .use(branch('!index.md')
            .use(permalinks({ relative: false }))
        )
    )
    .use(templates({ engine: 'jade', moment: moment }))

const logBuild = function (err) {
    if (err) { console.log(err); }
        else { console.log('OK'); }
}

if (!devMode) {
    pipeline
    .build(logBuild)
} else  {
    pipeline
    .use(serve({ host: '0.0.0.0', port: 8080, verbose: true }))
    .use(watch({ pattern: '**/*', livereload: true }))
    .build(logBuild)
}
