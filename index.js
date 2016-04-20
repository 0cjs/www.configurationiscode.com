'use strict'

const Metalsmith = require('metalsmith'),
    markdown = require('metalsmith-markdownit'),
    layouts = require('metalsmith-layouts')

Metalsmith(__dirname)
    .use(markdown())
    .use(layouts('handlebars'))
    .build(function(err) {
        if (err) throw err
    })
