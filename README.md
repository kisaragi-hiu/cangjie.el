# cangjie.el

`cangjie.el` provides one simple function to lookup the [Cangjie encoding](https://en.wikipedia.org/wiki/Cangjie_input_method) for a given [Han character](https://en.wikipedia.org/wiki/Chinese_characters).

## Install

Put `cangjie.el` under your `load-path`, then `(require 'cangjie)` in your init file.

Or use [`straight.el`](https://github.com/raxod502/straight.el):

```elisp
(straight-use-package '(cangjie.el :type git :host github
                                   :repo "kisaragi-hiu/cangjie.el"))
```

## Use

There is no interactive function yet, because I don't know how to do that.

For now, just write `(cangjie "字")` somewhere in a buffer and <kbd>M-x</kbd> `eval-last-sexp` on it.

# Naming

Calling this `cangjie.el` has the potential of being confused with an actual Cangjie input method in Emacs.
If that ends up being the case, this will be renamed to `cangjie-lookup.el`.
