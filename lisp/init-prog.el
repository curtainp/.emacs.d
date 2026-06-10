;;; -*- lexical-binding: t -*-

(use-package yasnippet
  :straight t
  :commands yas-global-mode
  :hook (after-init . yas-global-mode)
  :config
  (setq yas-triggers-in-field t
        yas-verbosity 0))

(use-package web-mode
  :straight t
  :defer t
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2))

(use-package embrace
  :straight t
  :commands embrace-commander
  :bind
  (:map global-map
        ("C-," . embrace-commander)))

(use-package css-mode
  :straight nil
  :defer t
  :config
  (setq css-indent-offset 2))

(use-package js
  :straight nil
  :defer t
  :config
  (setq js-indent-level 2))

(use-package smartparens
  :straight t
  :commands smartparens-global-mode
  :hook (after-init . smartparens-global-mode)
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-u" . sp-backward-up-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-p" . sp-backward-down-sexp)
              ("C-M-n" . sp-up-sexp)
              )
  :custom
  (sp-ignore-modes-list '(minibuffer-inactive-mode)) ; Enable in `minibuffer-mode'
  :config
  (require 'smartparens-config)
  (sp-local-pair 'org-mode "$" "$" :unless '(sp-point-after-word-p)))


(defvar custom-auto-langs '(bash c cpp css dockerfile html javascript json latex make org python rust sql toml
                             tsx typescript yaml xml markdown markdown-inline elisp))
(use-package treesit-auto
  :straight t
  :commands global-treesit-auto-mode
  :hook (after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-langs custom-auto-langs)
  :config
  (treesit-auto-add-to-auto-mode-alist custom-auto-langs))

(use-package electric
  :straight nil
  :commands (electric-indent-local-mode electric-pair-mode electric-quote-mode electric-indent-mode)
  :hook (prog-mode . electric-indent-local-mode)
  :config
  ;; only enable electric in `prog-mode'
  (electric-pair-mode -1)
  (electric-quote-mode -1)
  (electric-indent-mode -1))

(use-package paren
  :straight nil
  :commands show-paren-local-mode
  :hook (prog-mode . show-paren-local-mode)
  :config
  (custom-set-faces
   '(show-paren-match ((t :inherit 'bold))))
  (setq show-paren-style 'parenthesis
        show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t
        show-paren-context-when-offscreen 'overlay))

(use-package tex-mode
  :straight nil
  :mode "\\`\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'"
  :hook
  ((text-mode . turn-on-auto-fill)
   (prog-mode . (lambda () (setq-local sentence-end-double-space t))))
  :config
  (setq sentence-end-double-space nil
        sentence-end-without-period nil
        colon-double-space nil
        use-hard-newlines nil
        adaptive-fill-mode t))

;;;; Arch Linux and AUR package scripts
(use-package sh-script
  :straight nil
  :mode ("PKGBUILD" . sh-mode))

(use-package conf-mode
  :straight nil
  :mode ("\\`dircolors\\'" "\\.\\(service\\|timer\\)\\'"))

;;;; Emacs live documentation feedback
(use-package eldoc
  :straight nil
  :commands eldoc-mode
  :hook (prog-mode . eldoc-mode)
  :config
  (setq eldoc-message-function #'message))

;; [so-long] Workaround for long one-line file
(use-package so-long
  :straight nil
  :commands global-so-long-mode
  :hook ((after-init . global-so-long-mode)
	 ((so-long-mode prog-mode fundamental-mode) . +so-long-settings))
  :config
  ;; improve long line performance
  (defun +so-long-settings ()
    (setq bidi-display-reordering nil))

  ;; Saveplace should not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil)))

(use-package markdown-mode
  :straight t
  :defer t
  :config
  (setq markdown-enable-html t)
  ;; (markdown-enable-math t)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-highlighting-syntax t))

(use-package csv-mode
  :straight t
  :commands (csv-align-mode))

;; Highlight TODO keywords
(use-package hl-todo
  :straight t
  :commands hl-todo-mode
  :hook (prog-mode . hl-todo-mode)
  :config
  (cl-callf append hl-todo-keyword-faces
    '(("BUG"   . "#ee5555")
      ("FIX"   . "#0fa050")
      ("PROJ"  . "#447f44")
      ("IDEA"  . "#0fa050")
      ("INFO"  . "#0e9030")
      ("TWEAK" . "#fe9030")
      ("PERF"  . "#e09030"))))

(use-package rust-mode
  :straight t
  :defer t
  :init
  (setq rust-mode-treesitter-derive t
        rust-format-goto-problem nil
        rust-format-on-save t))

(use-package envrc
  :straight t
  :commands envrc-global-mode
  :hook (after-init . envrc-global-mode))

(use-package python
  :straight nil
  :defer t
  :custom
  (python-indent-guess-indent-offset t)
  ;; Don't emit warning
  (python-indent-guess-indent-offset-verbose nil))

(use-package aggressive-indent
  :straight t
  :commands (aggressive-indent-mode aggressive-indent-global-mode)
  :hook
  (lisp-mode . aggressive-indent-mode))

(use-package highlight-defined
  :straight t
  :commands highlight-defined-mode
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package hideshow
  :straight nil
  :commands hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
              ([C-tab] . hs-toggle-hiding))
  :config
  (setq hs-allow-nesting t))

(use-package eat
  :disabled
  :straight (:host codeberg :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el")))
  :commands eat)

(use-package ghostel
  :straight (:host github :repo "dakra/ghostel"
                   :files (:defaults "etc" "src" "vendor" "build.zig" "build.zig.zon" "symbols.map"))
  :commands (ghostel ghostel-compile-global-mode)
  :hook (emacs-startup . ghostel-compile-global-mode)
  :custom
  (ghostel-compile-global-mode-excluded-modes '(grep-mode rg-mode))
  :config
  (setq ghostel-enable-file-detection nil))

(use-package mermaid-mode
  :straight t)

(provide 'init-prog)
