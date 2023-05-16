;;; modaled.el --- Fully customizable modal editing for Emacs  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023  DCsunset
;;
;; Author: DCsunset
;; URL: https://github.com/DCsunset/modaled
;; Version: 0.1.0
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
	"Fully customizable modal editing for Emacs."
	:group 'editing
	:tag "Modaled"
	:prefix "modaled-"
	:link '(url-link :tag "GitHub" "https://github.com/DCsunset/modaled"))

(defvar modaled-default-state
  nil
  "Default modaled state.")

(defvar modaled-state
  nil
  "Current modaled state.")

(defun modaled--get-state-mode (state)
  "Get the symbol of STATE minor mode."
	(intern (format "modaled-%s-mode" state)))

(defun modaled--get-state-keymap (state)
  "Get the symbol of STATE keymap."
	(intern (format "modaled-%s-keymap" state)))

;;;###autoload
(defun modaled-set-state (state)
  "Set current modaled STATE."
  ; Do not use let as it only establishes bindings for values
  ; disable current mode
  (when modaled-state
    (funcall (modaled--get-state-mode modaled-state) 0))
  (when state
    (funcall (modaled--get-state-mode state) 1))
  (setq modaled-state state))

;;;###autoload
(defun modaled-define-state-keys (state &rest keybindings)
  "Define KEYBINDINGS for the STATE."
	(let ((keymap (modaled--get-state-keymap state)))
		(pcase-dolist (`(,key . ,def) keybindings)
			(eval `(define-key ,keymap (kbd ,key) #',def)))))

;;;###autoload
(defun modaled-define-global-keys (&rest keybindings)
  "Define KEYBINDINGS globally."
  (pcase-dolist (`(,key . ,def) keybindings)
    (global-set-key (kbd key) def)))

;;;###autoload
(defmacro modaled-define-state (state &rest body)
  "Define a new STATE minor mode.

This function will generate the definitions for the following items:
1. modaled-STATE-mode: Minor mode for the state.
2. modaled-STATE-keymap: Keymap for the state.

The following optional keywords are supported:
:sparse		Use a sparse keymap instead of a full keymap
:suppress	Remapping self-insert-command to undefined in the keymap
:lighter	Text displayed in the mode line when the state is active.
:cursor-type	Cursor type for the state."
  (let ((mode (modaled--get-state-mode state))
        (keymap (modaled--get-state-keymap state))
				(keymap-doc (format "Keymap for state %s." state))
				(sparse (plist-get body :sparse))
				(suppress (plist-get body :suppress))
        (lighter (plist-get body :lighter))
        (cursor-type (plist-get body :cursor-type))
        (doc (format "Modaled minor mode for state %s" state)))
    `(progn
			(defvar ,keymap
				(if ,sparse (make-sparse-keymap) (make-keymap))
				,keymap-doc)
			(when ,suppress
				(suppress-keymap ,keymap))
      (define-minor-mode ,mode
        ,doc
        :init-value nil
        :lighter ,lighter
        :keymap ,keymap
        (when ,cursor-type
					(setq-local cursor-type ,cursor-type))))))

;;;###autoload
(defmacro modaled-define-default-state (state)
  "Define default STATE used in global minor mode."
  (let ((mode (modaled--get-state-mode state)))
    `(progn
      (define-globalized-minor-mode modaled-global-mode
        ,mode
        (lambda ()
          (unless (minibufferp)
            ; enable default modaled minor modes
            (modaled-set-state ,state)))
	:group 'modaled)
      (setq modaled-default-state ,state))))

(provide 'modaled)

;;; modaled.el ends here
