# Modaled

Modaled is a fully customizable modal editing for Emacs.

Unlike many popular modal editing modes,
Modaled doesn't provide any keybindings
and nor does it define any minor modes itself.

This package provides util functions to help you build your own keybindings and minor modes.
It is inspired by [modalka](https://github.com/mrkkrp/modalka) but supports building your own minor modes.
You can define many different states and their corresponding minor modes in Modaled.

## Usage

You can define your own state and keybindings by `modaled-define-state <state> <keybindings>`.
It will create a minor mode `modaled-<state>-mode` and keymap `modaled-<state>-keymap` automatically.
You can change the keymap by `define-key` later and enable the minor mode whenever you want.

```emacs-lisp
(modaled-define-state "normal" '(
  ; disable current mode to insert text
  ("i" . (lambda () (interactive) (modaled-set-state nil)))
  ("h" . backward-char)
  ("l" . forward-char)
  ("k" . previous-line)
  ("j" . next-line)
))
```

To change the current state, you can use `modaled-set-state`:

```emacs-lisp
; Disable current state
(modaled-set-state nil)
; Enable/change a defined state
(modaled-set-state "normal")
```

To enable Modaled globally, you will need to define a default state by `modaled-define-default-state <state>`.
This will create a globalized minor mode `modaled-global-mode` for the default state minor mode you specified.
You can later enable the global mode directly and bind a key to enable the default mode.
Note that the default state must be defined by `modaled-define-state` before you call `modaled-define-default-state`.

```emacs-lisp
(modaled-define-default-state "normal")
(modaled-global-mode 1)
(global-set-key (kbd "<escape>") (lambda () (interactive) (modaled-set-state modaled-default-state)))
```

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
