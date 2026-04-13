;;; -*- lexical-binding: t -*-

(defgroup cw-emacs nil
  "Curtain Emacs Configuration Group."
  :group 'file)

(defcustom cw-emacs-user-name user-full-name
  "Set user full name within Emacs."
  :group 'cw-emacs
  :type 'string)

;; TODO: make this being a list of email address.
(defcustom cw-emacs-email-address "Y3VydGFpbndrQHByb3Rvbi5tZQ=="
  "Set user email address within Emacs."
  :group 'cw-emacs
  :type 'string)

(defcustom cw-emacs-notes-directory (expand-file-name "~/Documents/notes/")
  "Set org notes directory."
  :group 'cw-emacs
  :type 'string)

(defcustom cw-emacs-server-p nil
  "Enable `server-mode' or not."
  :group 'cw-emacs
  :type 'boolean)

(defcustom cw-emacs-lsp-client 'eglot
  "The LSP client to use.
`lsp-bridge' uses lsp-bridge with acm completion.
`eglot' uses eglot with corfu, cape, and emacs-lsp-booster."
  :group 'cw-emacs
  :type '(choice
          (const :tag "LSP Bridge" lsp-bridge)
          (const :tag "Eglot" eglot)))


(setq straight-repository-branch "develop")
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(mapc
 (lambda (path)
   (add-to-list 'load-path (locate-user-emacs-file path)))
 '("lisp" "modules"))

(require 'init-const)

(require 'init-basic)
(require 'init-ui)
(require 'init-evil)
(require 'init-completion)
(pcase cw-emacs-lsp-client
  ('lsp-bridge (require 'init-lsp-bridge))
  ('eglot      (require 'init-eglot))
  (_           (require 'init-eglot)))
(require 'init-nav)
(require 'init-search)
(require 'init-prog)

(require 'init-dired)
(require 'init-org)
(require 'init-latex)
(require 'init-notes)
(require 'init-docs)
(require 'init-tools)
(require 'init-rime)
(require 'init-vc)
