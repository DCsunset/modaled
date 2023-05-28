# Modaled

Modaled is a fully customizable modal editing package for Emacs.

Unlike many popular modal editing modes,
Modaled doesn't provide any keybindings
and nor does it come with any minor modes by default.

This package provides util functions to help you build your own keybindings and minor modes.
It is inspired by [modalka](https://github.com/mrkkrp/modalka) and [meow](https://github.com/meow-edit/meow),
but it supports building your own minor modes without any predefined states.
Compared to modalka, Modaled supports creating multiple minor modes and comes with no default mode,
which makes it possible to implement different states in modal editing and different keybindings for different situations.
Compared to meow, Modaled has no default config and greatly simplifies the design for customization from scratch.
You can freely define many different states and their corresponding minor modes in Modaled.

## Installation

To install the package manually, download this repo to Emacs' `load-path`.
Then add the following to the config file:

```emacs-lisp
(require 'modaled)
```

If you are using Nix home-manager,
you can import the `default.nix` in this repo,
and then add it to the emacs package:

```nix
programs.emacs = let
  modaled = import /path/to/modaled { inherit pkgs; };
in {
  enable = true;
  extraPackages = epkgs: [
    modaled
  ];
};
```

## Usage

You can define your own state and keybindings by `modaled-define-state STATE &rest BODY`.
You can set the lighter or cursor type in body.
It will create a minor mode `modaled-STATE-mode` and keymap `modaled-STATE-keymap` automatically.
You can set up the keymap by `modaled-define-keys` (or directly using `define-key`) and enable the minor mode whenever you want.

```emacs-lisp
(modaled-define-state "normal"
  :suppress t
  :lighter "[NOR]"
  :cursor-type 'box)
(modaled-define-keys "normal"
  '("h" . backward-char)
  '("l" . forward-char)
  '("k" . previous-line)
  '("j" . next-line)))
```

To change the current state, you can use `modaled-set-state` or `modaled-set-default-state`:

```emacs-lisp
; Disable all states
(modaled-set-state nil)
; Enable/change to a defined state
(modaled-set-state "normal")
; Reset to default state
(modaled-set-default-state)
```

To enable Modaled globally, you will need to define a default state by `modaled-define-default-state STATE`.
This will create a globalized minor mode `modaled-global-mode` for the default state minor mode you specified.
You can later enable the global mode directly using it.
Note that the default state must be defined by `modaled-define-state` before you call `modaled-define-default-state`.

```emacs-lisp
(modaled-define-state "insert"
  :sparse t
  :cursor-type 'bar
  :lighter "[INS]")

; modaled-define-keys also supports defining keys for multiple states (suppose select state is already defined)
(modaled-define-keys '("insert" "select")
  ; bind a key to change back to default state from other states
  `(,(kbd "ESC") . modaled-set-default-state))

(modaled-define-default-state "normal")
(modaled-global-mode 1)
```

To see supported arguments for each function, use `describe-function` (usually bound to `C-h f`) to see the docs.

## License

AGPL-3.0.

    Copyright (C) 2023  DCsunset

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published
    by the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
