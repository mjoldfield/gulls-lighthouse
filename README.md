# Gull’s Lighthouse

This repository contains a couple of supporting resources for
[an blog post](https://mjoldfield.com/atelier/atelier-dest/2017/10/gulls-lighthouse.html)
about [Gull’s Lighthouse](http://www.di.fc.ul.pt/~jpn/r/bugs/lighthouse.html).

## Interactive widget

This is written in [PureScript](http://www.purescript.org), which you’ll
need to install to run the code. There’s a [Getting Started Guide](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md) on the PureScript site.

Given the PureScript toolchain, you can compile my source to JavaScript
and then execute it thus:

    $ npm install
    $ bower install
    $ pulp browserfy -O to dist/Main.js
    $ open html/index.html

## Histograms

These are trivial little [Python](https://www.python.org) scripts using
[matplotlib](https://matplotlib.org).

   