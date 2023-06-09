;;; modaled.el --- Build your own minor modes for modal editing  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023  DCsunset
;;
;; Author: DCsunset
;; URL: https://github.com/DCsunset/modaled
;; Version: 0.4.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, modal-editing
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package helps build your own minor modes for modal editing in Emacs

;;; Code:

(defgroup modaled nil
  "Build your own minor modes for modal editing."
  :group 'editing
  :tag "Modaled"
  :prefix "modaled-"
  :link '(url-link :tag "GitHub" "https://github.com/DCsunset/modaled"))

(defmacro modaled-define-local-var (symbol &optional initvalue docstring)
  "Define SYMBOL as a permanent buffer-local variable.

Optional INITVALUE and DOCSTRING can be provided."
  (declare (indent defun)
           (doc-string 3))
  `(progn
     (defvar ,symbol ,initvalue ,docstring)
     (make-variable-buffer-local ',symbol)
     ; prevent it from being cleared by changing major mode
     (put ',symbol 'permanent-local t)))

(defvar modaled-default-state
  nil
  "Default modaled state.")

; make modaled-state buffer local as buffers have different current states
(modaled-define-local-var modaled-state
  nil
  "Current modaled state.")

(defun modaled--get-state-mode (state)
  "Get the symbol of STATE minor mode."
  (intern (format "modaled-%s-state-mode" state)))

(defun modaled--get-state-keymap (state)
  "Get the symbol of STATE keymap."
  (intern (format "modaled-%s-state-keymap" state)))

(defun modaled--get-substate-mode (substate)
  "Get the symbol of SUBSTATE minor mode."
  (intern (format "modaled-%s-substate-mode" substate)))

(defun modaled--get-substate-keymap (substate)
  "Get the symbol of SUBSTATE keymap."
  (intern (format "modaled-%s-substate-keymap" substate)))

(defun modaled--define-keymap (keymaps keybindings)
  "Define KEYBINDINGS for all the KEYMAPS."
  (dolist (keymap keymaps)
    (pcase-dolist (`(,key . ,def) keybindings)
      (define-key (symbol-value keymap) key def))))

;;;###autoload
(defun modaled-set-state (state)
  "Set current modaled STATE."
  ; prevent disabling and enabling the same state
  (when (and modaled-state (not (equal modaled-state state)))
    ; disable current mode
    (funcall (modaled--get-state-mode modaled-state) -1))
  (when state
    (funcall (modaled--get-state-mode state) 1))
  (setq modaled-state state))

;;;###autoload
(defun modaled-set-default-state ()
  "Set current state to default state."
  (interactive)
  (modaled-set-state modaled-default-state))

;;;###autoload
(defun modaled-define-state-keys (state &rest keybindings)
  "Define KEYBINDINGS for the STATE.

STATE can be a single state or a list of states.
If it's a list, KEYBINDINGS will be applied to all states in list."
  (declare (indent defun))
  (let ((states (if (listp state) state `(,state))))
    (modaled--define-keymap (mapcar #'modaled--get-state-keymap states) keybindings)))

;;;###autoload
(defun modaled-define-substate-keys (substate &rest keybindings)
  "Define KEYBINDINGS for the SUBSTATE.

SUBSTATE can be a single substate or a list of substates.
If it's a list, KEYBINDINGS will be applied to all substates in list."
  (declare (indent defun))
  (let ((states (if (listp substate) substate `(,substate))))
    (modaled--define-keymap (mapcar #'modaled--get-substate-keymap states) keybindings)))

;;;###autoload
(defun modaled-define-global-keys (&rest keybindings)
  "Define KEYBINDINGS globally."
  (declare (indent 0))
  (pcase-dolist (`(,key . ,def) keybindings)
    (global-set-key key def)))

;;;###autoload
(defmacro modaled--define-minor-mode (mode keymap body)
  "Define a minor MODE with KEYMAP and options in BODY.

The following options are supported for the minor mode:
:sparse   Use a sparse keymap instead of a full keymap.
:suppress Remapping `self-insert-command' to `undefined' in the keymap.
:lighter  Text displayed in the mode line when the state is active.
:cursor-type  Cursor type for the state."
  (let ((mode-name (symbol-name mode))
        (sparse (plist-get body :sparse))
        (suppress (plist-get body :suppress))
        (lighter (plist-get body :lighter))
        (cursor-type (plist-get body :cursor-type)))
    `(progn
      (defvar ,keymap
        (if ,sparse (make-sparse-keymap) (make-keymap))
        ,(format "Keymap for %s." mode-name))
      (when ,suppress
        (suppress-keymap ,keymap))
      (define-minor-mode ,mode
        ,(format "Modaled minor mode %s" mode-name)
        :lighter ,lighter
        :keymap ,keymap
        (when ,cursor-type
          (setq-local cursor-type ,cursor-type))))))

;;;###autoload
(defmacro modaled-define-state (state &rest body)
  "Define a new STATE minor mode with options in BODY.

STATE minor modes are managed and only one should be active.
You should change the state by `modaled-set-state' or
`modaled-set-default-state'.

This function will generate the definitions for the following items:
1. modaled-STATE-state-mode: Minor mode for the state.
2. modaled-STATE-state-keymap: Keymap for the state.

The following options are supported:
:sparse   Use a sparse keymap instead of a full keymap.
:suppress Remapping `self-insert-command' to `undefined' in the keymap.
:lighter  Text displayed in the mode line when the state is active.
:cursor-type  Cursor type for the state."
  (declare (indent defun))
  (let ((mode (modaled--get-state-mode state))
        (keymap (modaled--get-state-keymap state)))
    `(modaled--define-minor-mode ,mode ,keymap ,body)))

;;;###autoload
(defmacro modaled-define-substate (substate &rest body)
  "Define a new SUBSTATE minor mode with options in BODY.

SUBSTATE minor modes are unmanaged and multiple substates can be active.
You can enable/disable it by calling the minor mode function directly.

This function will generate the definitions for the following items:
1. modaled-STATE-substate-mode: Minor mode for the state.
2. modaled-STATE-substate-keymap: Keymap for the state.

The following options are supported:
:sparse   Use a sparse keymap instead of a full keymap.
:suppress Remapping `self-insert-command' to `undefined' in the keymap.
:lighter  Text displayed in the mode line when the state is active.
:cursor-type  Cursor type for the state."
  (declare (indent defun))
  (let ((mode (modaled--get-substate-mode substate))
        (keymap (modaled--get-substate-keymap substate)))
    `(modaled--define-minor-mode ,mode ,keymap ,body)))

;;;###autoload
(defmacro modaled-define-default-state (state &rest body)
  "Define default STATE used in global minor mode with options in BODY.

This function accept the following option:
:predicate  Specify which major modes the default state should be enabled in.
            It is directly passed to `define-globalized-minor-mode'."
  (declare (indent defun))
  (let ((mode (modaled--get-state-mode state))
        (pred (if (plist-member body :predicate)
                  `(:predicate ,(plist-get body :predicate))
                '())))
    `(progn
       (setq modaled-default-state ,state)
       (define-globalized-minor-mode modaled-global-mode
         ,mode
         (lambda ()
           (unless (minibufferp)
             ; enable default modaled minor modes
             (modaled-set-default-state)))
         :require 'modaled
         :group 'modaled
         ,@pred))))

(provide 'modaled)

;;; modaled.el ends here
