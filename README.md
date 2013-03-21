flymake-gjshint.el
=================

[![Build Status](https://travis-ci.org/yasuyk/flymake-gjshint-el.png?branch=master)](https://travis-ci.org/yasuyk/flymake-gjshint-el)

An Emacs flymake handler for syntax-checking Javascript source code
using [`jshint`][jshint] and [`gjslint`][gjslint].

Installation
=============

If you choose not to use one of the convenient packages in
[Melpa][melpa], you'll need to add the
directory containing `flymake-gjshint.el` to your `load-path`, and then `(require 'flymake-gjshint)`.

Usage
=====

Add the following to your emacs init file:

    (require 'flymake-gjshint) ;; Not necessary if using ELPA package
    (add-hook 'js-mode-hook 'flymake-gjshint:load)

If you want to disable flymake-gjshint in a certain directory
 (e.g. test code directory), set `flymake-gjshint` to nil in `.dir-locals.el`.

Here’s an example of a `.dir-locals.el` file:

    ((nil . ((flymake-gjshint . nil))))


[gjslint]:https://developers.google.com/closure/utilities/docs/linter_howto
[jshint]:http://www.jshint.com
[melpa]: http://melpa.milkbox.net

