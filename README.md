# cangjie.el
[![License: GPL v3+](https://img.shields.io/badge/License-GPL%20v3+-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/cangjie-badge.svg)](https://melpa.org/#/cangjie)

Lookup the [Cangjie encoding](https://en.wikipedia.org/wiki/Cangjie_input_method) for a given [Han character](https://en.wikipedia.org/wiki/Chinese_characters).

## Install

`cangjie.el` is available on MELPA.

### `use-package`

```elisp
(use-package cangjie)
```

### [`straight.el`](https://github.com/raxod502/straight.el)

```elisp
(straight-use-package 'cangjie)
```

### Manually

Put `cangjie.el` under your `load-path`, then `(require 'cangjie)` in your init file.

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
- `wiktionary-raw`,
  to output the line with the Cangjie code on the character's Wiktionary page,
- `'wiktionary`, or anything else,
  to grep the Wiktionary page like `'wiktionary-raw`, then try to remove the
  markup in the result, leaving just the Cangjie code.

# Credits

This is the first standalone Emacs package I've written, so I needed some directions.
[emacs-powerthesaurus](https://github.com/SavchenkoValeriy/emacs-powerthesaurus) and
[`define-word`](https://github.com/abo-abo/define-word) served as really good references
for me to follow.

I'm using the Cangjie dictionary from [RIME](//rime.im), [`rime-cangjie`](https://github.com/rime/rime-cangjie).
