;;; -*- lexical-binding: t -*-

(defvar custom-auto-langs '(bash c cpp css dockerfile html javascript json latex make org python rust sql toml
                             tsx typescript yaml xml markdown markdown-inline elisp))
(use-package treesit-auto
  :straight t
  :hook (after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-langs custom-auto-langs)
  :init
  (setq treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist custom-auto-langs))

(use-package emacs
  :straight nil
  :config
  (setq tab-always-indent 'complete) ; try indent first, if indent already, try complete
  (setq tab-first-completion 'word-or-paren-or-punct)
  (setq-default tab-width 4
                indent-tabs-mode nil))

(use-package electric
  :straight nil
  :hook (prog-mode . electric-indent-local-mode)
  :config
  ;; only enable electric in `prog-mode'
  (electric-pair-mode -1)
  (electric-quote-mode -1)
  (electric-indent-mode -1))

(use-package paren
  :straight nil
  :hook (prog-mode . show-paren-local-mode)
  :config
  (custom-set-faces
   '(show-paren-match ((t :inherit 'bold))))
  (setq show-paren-style 'parenthesis
        show-paren-when-point-in-periphery nil
        show-paren-when-point-inside-paren nil
        show-paren-context-when-offscreen 'overlay))

(use-package elisp-plus
  :straight (:host github :repo "abougouffa/elisp-plus")
  :init
  (elisp-plus-mode 1))

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
  :hook (prog-mode . eldoc-mode)
  :config
  (setq eldoc-message-function #'message))

;; [so-long] Workaround for long one-line file
(use-package so-long
  :straight nil
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
  :custom
  (markdown-enable-html t)
  ;; (markdown-enable-math t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-enable-highlighting-syntax t))

(use-package csv-mode
  :straight t
  :commands (csv-align-mode))

(use-package flyspell
  :disabled
  :straight nil
  :bind
  (:map flyspell-mode-map
        ("C-;" . nil)
        :map flyspell-mouse-map
        ("<mouse-3>" . flyspell-correct-word)
        :map ctl-x-x-map
        ("s" . flyspell-mode))
  :hook
  (LaTeX-mode org-mode)
  :config
  (setq flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil
        ispell-program-name "aspell"))

(use-package jinx
  :disabled
  :straight t
  :hook (emacs-startup . global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct)
  :config
  (setq jinx-languages "en_US-large")
  (add-to-list 'jinx-exclude-regexps '(t "\\cc")) ;ignore chinese check
  (add-to-list 'jinx-exclude-faces
               '(latex-mode font-lock-constant-face
                            font-lock-comment-face)))

;; Highlight TODO keywords
(use-package hl-todo
  :straight t
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
  ;; :mode ("\\.rs\\'" . rust-ts-mode)
  :init
  (setq rust-mode-treesitter-derive t
        rust-format-goto-problem nil
        rust-format-on-save t))

(use-package pyvenv
  :disabled
  :straight t
  :hook (pyvenv-post-activate . (lambda ()
                                  (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3"))))
  :hook (pyvenv-post-deactivate . (lambda ()
                                    (setq python-shell-interpreter "python3")))
  :init
  (pyvenv-mode t))

(use-package pyvenv-auto
  :disabled
  :straight t
  :hook (python-mode . pyvenv-auto-mode))

(use-package envrc
  :straight t
  :hook (after-init . envrc-global-mode))

(use-package python
  :straight nil
  :custom
  (python-indent-guess-indent-offset t)
  ;; Don't emit warning
  (python-indent-guess-indent-offset-verbose nil))

(use-package kdl-ts-mode
  :disabled
  :straight (:type git :host github :repo "merrickluo/kdl-ts-mode")
  :config
  (setq kdl-ts-mode-indent-offset 4))


(provide 'init-prog)
