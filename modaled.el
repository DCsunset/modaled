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

(defvar modaled--state-alist
  '()
  "Alist of all defined states.")

;;;###autoload
(defun modaled-get-state-mode (state)
  "Get minor mode of STATE."
  ; do not use alist-get as it uses eq instead of equal
  (cdr (assoc state modaled--state-alist)))

;;;###autoload
(defun modaled-set-state (state)
  "Set current modaled STATE."
  ; Do not use let as it only establishes bindings for values
  ; disable current mode
  (when modaled-state
    (funcall (modaled-get-state-mode modaled-state) 0))
  (when state
    (funcall (modaled-get-state-mode state) 1))
  (setq modaled-state state))

;;;###autoload
(defun modaled-define-keys (keymap keybindings)
  "Define KEYBINDINGS in the KEYMAP."
  (pcase-dolist (`(,key . ,def) keybindings)
    (define-key keymap (kbd key) def)))

;;;###autoload
(defmacro modaled-define-state (state keybindings)
  "Define a new STATE mode and its KEYBINDINGS.

Generated minor mode: modaled-STATE-mode.
Generated keymap: modaled-STATE-keymap."
  (let ((mode (intern (format "modaled-%s-mode" state)))
        (keymap (intern (format "modaled-%s-keymap" state)))
        (lighter (format "[%s]" state))
        (doc (format "Modaled state minor mode %s" state)))
    `(progn
      (defvar ,keymap (make-sparse-keymap) "")
      (modaled-define-keys ,keymap ,keybindings)
      (define-minor-mode ,mode
        ,doc
        :init-value nil
        :lighter ,lighter
        :keymap ,keymap
        (setq-local cursor-type 'box))
      (setq modaled--state-alist (push (cons ,state ',mode) modaled--state-alist)))))

;;;###autoload
(defmacro modaled-define-default-state (state)
  "Define default STATE used in global minor mode."
  (let ((mode (intern-soft (format "modaled-%s-mode" state))))
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
