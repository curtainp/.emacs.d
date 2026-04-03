;;; -*- lexical-binding: t -*-

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
(require 'init-custom)

(require 'init-basic)
(require 'init-ui)
(require 'init-evil)
(require 'init-completion)
(pcase curtain-lsp-client
  ('lsp-bridge (require 'init-lsp-bridge))
  ('eglot      (require 'init-eglot))
  (_           (require 'init-lsp-bridge)))
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
