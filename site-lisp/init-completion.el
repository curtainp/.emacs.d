;; -*- lexical-binding: t; -*-

(setq completion-styles '(basic substring initials flex orderless)
      completion-category-defaults nil
      completion-ignore-case t
      completions-format 'one-column
      completion-show-help nil
      completion-auto-help 'always
      completion-auto-select nil
      completions-detailed t
      completion-show-inline-help nil
      completions-max-height 6
      completions-header-format (propertize "%s candidates:\n" 'face 'bold-italic)
      completions-highlight-face 'completions-highlight
      minibuffer-completion-auto-choose t
      completions-sort 'historical
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-category-overrides '((file (styles . (basic partial-completion orderless)))
				      (bookmark (styles . (basic substring)))
				      (library (styles . (basic substring)))
				      (embark-keybinding (styles . (basic substring)))
				      (imenu (styles . (basic substring orderless)))
				      (consult-location (styles . (basic substring orderless)))
				      (kill-ring (styles . (emacs22 orderless)))
				      (eglot (styles . (emacs22 substring orderless)))))

(setq-default case-fold-search t)

(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 100)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-to-list 'savehist-additional-variables 'kill-ring))

(use-package dabbrev
  :commands (dabbrev-expand dabbrev-completion)
  :config
;;;; `dabbrev' (dynamic word completion (dynamic abbreviations))
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t)
  (setq dabbrev-ignored-buffer-modes
        '(archive-mode image-mode docview-mode pdf-view-mode)))

(use-package vertico
  :custom (vertico-count 15)
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-cycle t
	vertico-resize nil)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (define-key vertico-map [backspace] #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "C-'")  #'vertico-quick-jump)
  )

(use-package marginalia
  :hook
  (after-init . marginalia-mode))

;; copy from https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-lisp/prot-orderless.el
(defun prot-orderless-literal (word _index _total)
  "Read WORD= as a literal string."
  (when (string-suffix-p "=" word)
    ;; The `orderless-literal' is how this should be treated by
    ;; orderless.  The `substring' form omits the `=' from the
    ;; pattern.
    `(orderless-literal . ,(substring word 0 -1))))

(defun prot-orderless-file-ext (word _index _total)
  "Expand WORD. to a file suffix when completing file names."
  (when (and minibuffer-completing-file-name
             (string-suffix-p "." word))
    `(orderless-regexp . ,(format "\\.%s\\'" (substring word 0 -1)))))

(defun prot-orderless-beg-or-end (word _index _total)
  "Expand WORD~ to \\(^WORD\\|WORD$\\)."
  (when-let (((string-suffix-p "~" word))
             (word (substring word 0 -1)))
    `(orderless-regexp . ,(format "\\(^%s\\|%s$\\)" word word))))

(use-package orderless
  :demand t ;; prevent lazyload which cause below undefined
  :config
  (setq orderless-matching-styles '(orderless-prefixes orderless-regexp)
	orderless-style-dispatchers '(prot-orderless-literal
				      prot-orderless-file-ext
				      prot-orderless-beg-or-end))
  :bind (:map minibuffer-local-completion-map
	      ("SPC" . nil) ;; use it for `orderless' group
	      ("?" . nil)))

(use-package consult
  :demand t
  :bind (:map global-map
	 ("C-c M-x" . consult-mode-command)
	 ("C-c r"   . consult-ripgrep)
	 ("C-c i"   . consult-info)
	 ("C-c m"   . consult-man)
	 ("C-c f"   . consult-fd)
	 ("C-c T"   . consult-theme)
	 ("C-."     . consult-imenu)
	 ("C-c c f" . describe-face)
	 ("C-x b"   . consult-buffer)
	 ([remap Info-search]      . consult-info)
	 ([remap goto-line]        . consult-goto-line)
	 ([remap isearch-forward]  . consult-line)
	 ("M-y"     . consult-yank-pop))
  :init
  (advice-add #'multi-occur :override #'consult-multi-occur)
  :config
  (setq
   consult-narrow-key "<"
   consult-line-numbers-widen t
   consult-async-min-input 2
   consult-async-refresh-delay 0.15
   consult-async-input-throttle 0.2
   consult-async-input-debounce 0.1
   consult-line-start-from-top t
   )
  )

(use-package embark
  :bind (("s-."   . embark-act)
	 ("M-."   . embark-dwim)  ; NOTE: overrides `xref-find-definition'
	 ([remap describe-bindings] . embark-bindings))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
(use-package embark-consult
  :bind (:map minibuffer-mode-map
	      ("C-c C-o" . embark-export))
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; buffer completion
(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  (corfu-auto-delay 0.08)
  (corfu-popupinfo-delay '(0.2 . 0.1))
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind ("M-/" . completion-at-point)
  :config
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  :hook ((after-init . global-corfu-mode)
	 (global-corfu-mode . corfu-popupinfo-mode))
  )

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))


(provide 'init-completion)
