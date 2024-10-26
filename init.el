;; -*- lexical-binding: t ; -*-

(setq custom-file "~/.emacs.d/.emacs.custom.el")

(setq warning-minimum-level :emergency) ; ignore native comp warning

(defconst cs/is-mac (eq system-type 'darwin))
(defconst cs/is-linux (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst cs/org-path "~/workspace/docs/org")
(defconst cs/fallback-fonts '("Jigmo" "Jigmo2" "Jigmo3"))
(defconst cs/emoji-fonts '("Apple Color Emoji"
			   "Noto Color Emoji"
			   "Noto Emoji"
			   "Segoe UI Emoji"
			   "Symbola"))
(defconst cs/default-font "IosevkaTerm Nerd Font 16")
(defconst cs/zh-default-font "LXGW WenKai")
(defconst cs/symbol-default-font "Symbols Nerd Font Mono")

(when cs/is-mac
  (setq mac-option-modifier 'super
	mac-command-modifier 'meta
	mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  (defconst +env-file (concat user-emacs-directory ".env"))
  (defun +load-env-file (file &optional noerror)
    "Read and set envvars from FILE.
If NOERROR is non-nil, don't throw an error if the file doesn't exist or is
unreadable. Returns the names of envvars that were changed."
    (if (not (file-readable-p file))
        (unless noerror
          (signal 'file-error (list "Couldn't read envvar file" file)))
      (let (envvars environment)
        (with-temp-buffer
          (save-excursion
            (insert "\n")
            (insert-file-contents file))
          (while (re-search-forward "\n *\\([^#= \n]*\\)=" nil t)
            (push (match-string 1) envvars)
            (push (buffer-substring
                   (match-beginning 1)
                   (1- (or (save-excursion
                             (when (re-search-forward "^\\([^= ]+\\)=" nil t)
                               (line-beginning-position)))
                           (point-max))))
                  environment)))
        (when environment
          (setq process-environment
                (append (nreverse environment) process-environment)
                exec-path
                (if (member "PATH" envvars)
                    (append (split-string (getenv "PATH") path-separator t)
                            (list exec-directory))
                  exec-path)
                shell-file-name
                (if (member "SHELL" envvars)
                    (or (getenv "SHELL") shell-file-name)
                  shell-file-name))
          envvars))))
  (when (and (or (display-graphic-p))
	     (file-exists-p +env-file))
    (+load-env-file +env-file)))
  
(column-number-mode 1)
(show-paren-mode 1)
(dolist (mode '(text-mode-hook
		prog-mode-hook
		conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))


(add-to-list 'load-path "~/.emacs.d/site-lisp/")
;; use simpc-mode to substitude the fucking stupid c-mode
(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

;; duplicate line
(setq duplicate-line-final-position 1)
(global-set-key (kbd "C-,") 'duplicate-line)
(global-set-key (kbd "C-;") 'compile)
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
      ring-bell-function 'ignore
      use-short-answers t
      word-wrap-by-category t
      use-file-dialog nil
      use-dialog-box nil
      window-resize-pixelwise t
      frame-resize-pixelwise t
      indicate-buffer-boundaries 'left
      scroll-preserve-screen-position 'always
      truncate-partial-width-windows nil
      history-length 1000
      indent-tabs-mode nil
      save-interprogram-paste-before-kill t)

;; Show directory first
(setq dired-listing-switches "-alh")
;; Always delete and copy recursively
(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always)

(global-auto-revert-mode t)
 
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
	recentf-exclude (list "\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                                   "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                                   "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                                   (lambda (file) (file-in-directory-p file package-user-dir))
                                   (expand-file-name recentf-save-file))
	recentf-save-file-modes nil
	recentf-keep nil
	recentf-auto-cleanup nil
	recentf-initialize-file-name-history nil
	recentf-filename-handlers nil
	recentf-show-file-shortcuts-flag nil))

(use-package hungry-delete
  :init
  (global-hungry-delete-mode))

(use-package smartparens
  :init
  (smartparens-global-mode t)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'rust-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil))

;;;; Eldoc (Emacs live documentation feedback)
(use-package eldoc
  :ensure nil
  :hook (prog-mode . eldoc-mode)
  :config
  (setq eldoc-message-function #'message)) ; don't use mode line for M-x eval-expression, etc.

(use-package vterm
  :bind (:map vterm-mode-map
	      ([return] . vterm-send-return))
  :hook (vterm-mode . (lambda ()
			(set (make-local-variable 'buffer-face-mode-face) '(:family "IosevkaTerm Nerd Font"))
			(buffer-face-mode t)))
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 10000)
  )

(use-package multi-vterm
  :bind (([remap project-shell] . multi-vterm-project)
	 ([f1] . multi-vterm-project))
  :custom (multi-vterm-dedicated-window-height-percent 30))

;; #####################################
;; snippets
;; #####################################
(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
	 (org-mode . yas-minor-mode))
  :config
  (yas-reload-all))

;; (use-package yasnippet-snippets)
;; (use-package yasnippet-capf
;;   :init (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(require 'init-ui)
(require 'init-completion)
(require 'init-markdown)
(require 'init-org)
(require 'init-vcs)
(require 'init-lsp)
(require 'init-multimedia)
(require 'init-prog)
(require 'init-eshell)
(require 'init-elisp)
(require 'init-rust)
(require 'init-web)
(require 'init-tools)
