;;; -*- lexical-binding: t -*-

(require 'embark)

(defvar-keymap curt-embark-general-map
  :parent embark-general-map
  "i" #'embark-insert
  "w" #'embark-copy-as-kill
  "E" #'embark-export
  "S" #'embark-collect
  "A" #'embark-act-all
  "DEL" #'delete-region)

(defvar-keymap curt-embark-url-map
  :parent embark-general-map
  "b" #'browse-url
  "d" #'embark-download-url
  "e" #'eww)

(defvar-keymap curt-embark-buffer-map
  :parent embark-general-map
  "k" #'kill-buffer
  "o" #'switch-to-buffer-other-window
  "e" #'ediff-buffers)

(add-to-list 'embark-post-action-hooks (list 'kill-buffer 'embark--restart))

(defvar-keymap curt-embark-file-map
  :parent embark-general-map
  "f" #'find-file
  "j" #'embark-dired-jump
  "c" #'copy-file
  "e" #'ediff-files)

(defvar-keymap curt-embark-identifier-map
  :parent embark-general-map
  "h" #'display-local-help
  "." #'xref-find-definitions
  "o" #'occur)

(defvar-keymap curt-embark-command-map
  :parent embark-general-map
  "h" #'describe-command
  "." #'embark-find-definition)

(defvar-keymap curt-embark-expression-map
  :parent embark-general-map
  "e" #'pp-eval-expression
  "m" #'pp-macroexpand-expression)

(defvar-keymap curt-embark-function-map
  :parent embark-general-map
  "h" #'describe-function
  "." #'embark-find-definition)

(defvar-keymap curt-embark-package-map
  :parent embark-general-map
  "h" #'describe-package
  "i" #'package-install
  "d" #'package-delete
  "r" #'package-reinstall
  "u" #'embark-browse-package-url
  "w" #'embark-save-package-url)

(defvar-keymap curt-embark-symbol-map
  :parent embark-general-map
  "h" #'describe-symbol
  "." #'embark-find-definition)

(defvar-keymap curt-embark-variable-map
  :parent embark-general-map
  "h" #'describe-variable
  "." #'embark-find-definition)

(defvar-keymap curt-embark-region-map
  :parent embark-general-map
  "a" #'align-regexp
  "D" #'delete-duplicate-lines
  "f" #'flush-lines
  "i" #'epa-import-keys-region
  "d" #'epa-decrypt-armor-in-region
  "r" #'repunctuate-sentences
  "s" #'sort-lines
  "u" #'untabify)

;; The minimal indicator shows cycling options, but I have no use
;; for those.  I want it to be silent.
(defun curt-embark-no-minimal-indicator ())
(advice-add #'embark-minimal-indicator :override #'curt-embark-no-minimal-indicator)

(defun curt-embark-act-no-quit ()
  "Call `embark-act' but do not quit after the action."
  (interactive)
  (let ((embark-quit-after-action nil))
    (call-interactively #'embark-act)))

(defun curt-embark-act-quit ()
  "Call `embark-act' and quit after the action."
  (interactive)
  (let ((embark-quit-after-action t))
    (call-interactively #'embark-act))
  (when (and (> (minibuffer-depth) 0)
             (derived-mode-p 'completion-list-mode))
    (abort-recursive-edit)))

(provide 'curt-embark)
