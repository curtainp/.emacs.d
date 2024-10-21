;; -*- lexical-binding: t ; -*-

;; Temp: Explicitly set PATH environment variable and update exec-path to match it.
;; (the string here should be copied from the PATH in Emacs.app/Contents/Info.plist)
(setenv "PATH" "/opt/homebrew/bin:/opt/homebrew/sbin:/Users/curtainw/.pyenv/shims:/Users/curtainw/.cargo/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Applications/kitty.app/Contents/MacOS")
(setq exec-path (split-string (getenv "PATH") path-separator))

(setq custom-file "~/.emacs.d/.emacs.custom.el")

(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)
(global-display-line-numbers-mode)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")
;; use simpc-mode to substitude the fucking stupid c-mode
(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

;; duplicate line
(setq duplicate-line-final-position 1)
(global-set-key (kbd "C-,") 'duplicate-line)
(global-set-key (kbd "C-x p") 'previous-buffer)
(global-set-key (kbd "C-x n") 'next-buffer)
(global-set-key (kbd "C--") 'set-mark-command)

;; straight.el for package manager
(setq straight-use-package-by-default t
      use-package-always-defer t
      straight-cache-autoloads t
      straight-vc-git-default-clone-depth 1
      straight-check-for-modifications '(find-when-checking)
      )
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
(straight-use-package 'use-package)

;; #####################################
;; default behavior
;; #####################################
(setq initial-frame-alist '((fullscreen . maximized)))
(setq create-lockfiles nil
      make-backup-files nil
      auto-save-default nil
      use-dialog-box nil
      use-file-dialog nil
      ring-bell-function 'ignore
      use-short-answers t
      mac-option-modifier 'super
      mac-command-modifier 'meta
      )

;; Show directory first
(setq dired-listing-switches "-alh")
;; Always delete and copy recursively
(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always)
 
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Shows icons
(use-package nerd-icons-dired
  :custom-face
  (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
  :hook (dired-mode . nerd-icons-dired-mode))
  
;; (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

(prefer-coding-system 'utf-8)
(setq fast-but-imprecise-scrolling t
      jit-lock-defer-time 0)

(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent))

(use-package proced
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t))

(use-package recentf
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 100
	recentf-max-menu-items 25
	recentf-save-file-modes nil
	recentf-keep nil
	recentf-auto-cleanup nil
	recentf-initialize-file-name-history nil
	recentf-filename-handlers nil
	recentf-show-file-shortcuts-flag nil))

(use-package hungry-delete
  :init
  (global-hungry-delete-mode))

;;;; Eldoc (Emacs live documentation feedback)
(use-package eldoc
  :ensure nil
  :hook (prog-mode . eldoc-mode)
  :config
  (setq eldoc-message-function #'message)) ; don't use mode line for M-x eval-expression, etc.

;; #####################################
;; snippets
;; #####################################
(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
	 (org-mode . yas-minor-mode)))

(use-package yasnippet-snippets)
(use-package yasnippet-capf
  :init (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(require 'init-ui)
(require 'init-completion)
(require 'init-markdown)
(require 'init-org)
(require 'init-vcs)
;; (require 'init-lsp)
(require 'init-prog)
(require 'init-eshell)
(require 'init-elisp)
(require 'init-rust)
(require 'init-web)
