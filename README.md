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

Do <kbd>M-x</kbd> `cangjie`, then enter the Han character in the prompt.
The result will be `message`d out.

When called non-interactively, take the character as the argument, and return the Cangjie code.

# Thanks

This is the first standalone Emacs package I've written, so I needed some directions.
[emacs-powerthesaurus](https://github.com/SavchenkoValeriy/emacs-powerthesaurus) and
[`define-word`](https://github.com/abo-abo/define-word) served as really good references
for me to follow, and I want to thank them here.
