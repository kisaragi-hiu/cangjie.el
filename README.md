# cangjie.el

Lookup the [Cangjie encoding](https://en.wikipedia.org/wiki/Cangjie_input_method) for a given [Han character](https://en.wikipedia.org/wiki/Chinese_characters).

## Install

Put `cangjie.el` under your `load-path`, then `(require 'cangjie)` in your init file.

Or use [`straight.el`](https://github.com/raxod502/straight.el):

```elisp
(straight-use-package '(cangjie.el :type git :host github
                                   :repo "kisaragi-hiu/cangjie.el"))
```

## Use

- `cangjie-at-point`: Lookup Cangjie encoding for character at point.
- `cangjie (CHARACTER)`: Lookup Cangjie encoding for CHARACTER (asked in prompt when called interactively).

## Customize

`cangjie-source`: the dictionary to be used by `cangjie`.

Its value can be:

- a path,
  which makes `cangjie` read from that path if it's a valid RIME dictionary.
- `'rime`,
  to download the dictionary from <https://github.com/rime/rime-cangjie>,
  and save it for future use.
- `'wiktionary-raw`,
  to do a simple `curl wiktionary.org/wiki/char | grep`.
- `'wiktionary`, or anything else,
  to grep the Wiktionary page like `'wiktionary-raw`, then try to remove the
  markup in the result, leaving just the Cangjie code.

# Credits

This is the first standalone Emacs package I've written, so I needed some directions.
[emacs-powerthesaurus](https://github.com/SavchenkoValeriy/emacs-powerthesaurus) and
[`define-word`](https://github.com/abo-abo/define-word) served as really good references
for me to follow.

I'm using the Cangjie dictionary from [RIME](//rime.im), [`rime-cangjie`](https://github.com/rime/rime-cangjie).
