# Modaled

[![MELPA](https://melpa.org/packages/modaled-badge.svg)](https://melpa.org/#/modaled)
[![License](https://badgen.net/github/license/dcsunset/modaled)](https://github.com/DCsunset/modaled)

Modaled is a fully customizable modal editing package for Emacs.

Unlike many popular modal editing modes,
Modaled doesn't provide any keybindings
and nor does it come with any minor modes by default.

This package provides util functions to help you build your own keybindings and minor modes.
It is inspired by [evil](https://github.com/emacs-evil/evil), [modalka](https://github.com/mrkkrp/modalka), and [meow](https://github.com/meow-edit/meow),
but it supports building your own minor modes without any predefined states.
Compared to modalka, Modaled supports creating multiple minor modes and comes with no default mode,
which makes it possible to implement different states in modal editing and different keybindings for different situations.
Compared to meow, Modaled has no default config and greatly simplifies the design for customization from scratch.
You can freely define many different states and their corresponding minor modes in Modaled.

## Installation

This package is available on MELPA.
You can install it by `M-x package-install RET modaled RET` or use [use-package](https://github.com/jwiegley/use-package) to install it.

To install the package manually, download this repo to Emacs' `load-path`.
Then add the following to the config file:

```emacs-lisp
(require 'modaled)
```

If you are using Nix, this package is also available in [NUR](https://nur.nix-community.org/repos/dcsunset/).

If you are using Nix flake,
you can directly install the latest version (main branch) to your inputs and use it in emacsPackages:
```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    modaled = {
      url = "github:DCsunset/modaled";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
    in {
      nixosConfigurations."<hostname>" = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          ({ config, pkgs, ... }: {
            home-manager.users."<user>" = {
              programs.emacs = {
                enable = true;
                extraPackages = epkgs: [
                  inputs.modaled.packages.${system}.default
                ];
              };
            };
          })
          ./configuration.nix
        ];
      };
    };
}
```


## Usage

### Basics

You can define your own state and keybindings by `modaled-define-state STATE &rest BODY`.
You can set the lighter or cursor type in body.
It will create a minor mode `modaled-STATE-state-mode` and keymap `modaled-STATE-state-keymap` automatically.

The mode is added to `emulation-mode-map-alists` by default to increase its priority unless `:no-emulation` is specified.

Besides, the keymap is suppressed by default, which means using undefined keys will result in an error instead of inserting the raw character
unless `:no-suppress` is set to true.

You can set up the keymap by `modaled-define-keys` (or directly using `define-key`) and enable the minor mode whenever you want.
(Note: `modaled-define-state-keys`, `modaled-define-substate-keys` and `modaled-define-global-keys` are now deprecated)

```emacs-lisp
(modaled-define-state "normal"
  ; use no-suppress to allow inserting the char if keybinding is not defined
  ; :no-suppress t
  ;; to prevent it from being added to emulation-mode-map-alist
  ; :no-emulation t
  :lighter "[NOR]"
  :cursor-type 'box)
; modaled-define-keys supports binding keys for multiple states or substates or globally
(modaled-define-keys
  :states '("normal")
  :bind
  `(("h" . backward-char)
    ("l" . forward-char)
    ("k" . previous-line)
    ("j" . next-line)
    ;; you can also bind multiple keys to a command
    (("a" "b") . ,(lambda () (interactive) (message "hello")))))

(modaled-define-state "insert"
  :sparse t
  ;; insert state must be no-suppress to support inserting char
  :no-suppress t
  :cursor-type 'bar
  :lighter "[INS]")
(modaled-define-keys
  :states '("insert" "normal")
  ; bind a key to change back to default state from other states
  :bind
  '(("<escape>" . modaled-set-default-state)))
```

To enable Modaled globally, you will need set init state and call `modaled-setup` function.
This will create hooks to enable the init state on buffer creation and major mode change.
You can set one global init using a custom function:
```emacs-lisp
;; set init state using a function
(setq modaled-init-state-fn
      (lambda ()
        (if (eq major-mode 'vterm-mode)
            "insert")
          "normal"))
;; enable globally based on init state
(modaled-setup)

;; manually switch to it
(modaled-set-init-state)
```

You can optionally set a function for main state.
It is different from the init state as it's not auto enabled on major mode change.
Instead, it is just a helper to easily switch to it:
```emacs-lisp
(setq modaled-main-state-fn (lambda () "normal"))
;; manually switch to it
(modaled-set-main-state)
```

To change the current state, you can use `modaled-set-state`, `modaled-set-default-state` or `modaled-set-main-state`:
```emacs-lisp
;; Disable all states
(modaled-set-state nil)
;; Enable/change to a defined state
(modaled-set-state "normal")
;; Reset to init state
(modaled-set-init-state)
;; Set to main state
(modaled-set-main-state)
```


To see supported arguments for each function, use `describe-function` (usually bound to `C-h f`) to see the docs.


### Substates

Modaled also supports defining substates.

In Modaled, states are managed like a major mode (which means only one state should be enabled),
while substates are unmanaged like a minor mode (multiple substates can be active at the same time).
The current state is stored in variable `modaled-state`.
You should only use `modaled-set-state` or `modaled-set-default-state` to change a state,
but you can enable substates by calling the minor mode function directly.

Similarly, substates are defined by `modaled-define-substate` and keybindings are defined by `modaled-define-keys`.
The function parameters are the same as those for states.
The corresponding minor mode and keymap are `modaled-SUBSTATE-substate-mode` and `modaled-SUBSTATE-substate-keymap`.

Substates are more flexible than states as you manage them directly.
There's also a helper function `modaled-enable-substate-on-state-change` to enable a substate when the state changes.
The example below shows how to define a substate only for org-mode and normal state:

```emacs-lisp
(modaled-define-substate "org")
(modaled-define-keys
  :substates '("org")
  :bind
  '(("SPC o" . org-open-at-point)))
; enable org substate when in normal state
(modaled-enable-substate-on-state-change
  "org"
  :states '("normal")
  :major '(org-mode))
```


## Migration

### Migration from v0.8 to v0.9

Starting from v0.9, modaled uses `keymap-set` instead of `define-key` when binding keys.
It means that the keys used in `:bind` must satisfy `key-valid-p` and `kbd` should no longer be used.

For example, the following keys should be changed:
- `[escape]` -> `"<escape>"`
- `(kbd "M-<return>")` -> `"M-<return>"`
- `"a b"` -> `"a SPC b"`


## License

AGPL-3.0.

    Copyright (C) 2023-2025  DCsunset

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
