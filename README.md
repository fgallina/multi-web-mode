# multi-web-mode

Multiple major mode support for web editing in Emacs.

## Info

+ Author: Fabián Ezequiel Gallina
+ Contact: fgallina at gnu dot org
+ Project homepage: http://github.com/fgallina/multi-web-mode
+ My Blog: http://www.from-the-cloud.com

Donations welcome!

[![Flattr this git repo](http://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=fgallina&url=https://github.com/fgallina/multi-web-mode&title=multi-web-mode&language=en_GB&tags=github&category=software)

## Introduction

Multi Web Mode is a minor mode which makes web editing in Emacs much
easier.

Basically it selects the appropriate major mode automatically when you
move the point and calculates the correct indentation of chunks
according to the indentation of the most relevant major mode.

Currently it is focused to work on buffers which contain html, css,
javascript and/or php, but adding other modes is really simple: you
just need to add your own mode to the `mweb-tags`. You can do that by
using M-x customize-group RET multi-web-mode and looking for the
`mweb-tags` variable.

All third packages were removed from the project repo (they were
included with mweb previously), mainly because it is difficult to
track new versions for everything and because it favored the user
laziness to check the basic configuration :).

The last multi-web-mode release which contained all packages was
0.2.1. It is still available from here:

+ http://github.com/fgallina/multi-web-mode/tree/0.2.1

## Requirements

You don't need any special libraries to run this package. Just the
ones you want to integrate to it.

Regarding the version of Emacs, this package was tested successfully
with >= 23.0.60.

## Installation

To install Multi Web Mode you'll need to do something like this in
your .emacs:

```emacs-lisp
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)```

## Usage

Multi Web Mode binds the following keystrokes:

+ `M-<f11>` : Prompts the user to override the default major mode.
+ `M-<f12>` : Prompts the user to override the calculated extra
  indentation. Useful when the automatic calculation is not good
  enough.

## Bug Reports

If you find a bug please report in the github tracker or sending an
email to the address listed at the top of the file.

## License

multi-web-mode.el is free software under the GPL v3, see LICENSE file
for details.
