;;; init-eglot.el -*- lexical-binding: t -*-

;; Eglot (built-in since Emacs 29)
(use-package eglot
  :straight nil
  :commands (eglot eglot-ensure)
  :hook ((python-base-mode
          rust-ts-mode
          c-ts-mode c++-ts-mode
          js-ts-mode typescript-ts-mode tsx-ts-mode
          bash-ts-mode
          css-ts-mode
          org-mode
          html-mode)
         . eglot-ensure)
  :bind
  (("C-c e i" . eglot-find-implementation)
   ("C-c e a" . eglot-code-actions)
   ("C-c e r" . eglot-rename))
  :config
  (add-to-list 'eglot-server-programs '(org-mode . ("harper-ls" "--stdio")))
  (setq-default eglot-workspace-configuration
                '(:harper-ls (:linters (:SpellCheck :json-false
                                                    :SentenceCapitalization :json-false
                                                    :Spaces nil))))
  (setq eglot-autoshutdown t
        eglot-events-buffer-config '(:size 0 :format full) ;; no log
        ;; Keep the server closer to the live buffer so completion
        ;; doesn't lag behind fast typing.
        eglot-send-changes-idle-time 0.05
        ;; format with `apheleia-format-buffer' instead.
        eglot-ignored-server-capabilities '(:documentFormattingProvider
                                            :documentRangeFormattingProvider)
        eglot-report-progress nil))

;; Eglot-booster: wraps emacs-lsp-booster for faster JSON processing
;; Requires `emacs-lsp-booster' binary in PATH
(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :custom
  ;; Emacs 31's native JSON reader is already fast, and avoiding
  ;; bytecode decoding sidesteps odd UTF-8 display glitches from some
  ;; completion items.
  (eglot-booster-io-only t)
  :config
  (eglot-booster-mode))

;; Corfu: in-buffer completion UI
(use-package corfu
  :straight t
  :commands (global-corfu-mode corfu-mode)
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  :config
  (defun +corfu-apply-theme (&rest _)
    "Sync Corfu faces with the active theme."
    (let* ((bg (face-background 'default nil t))
           (fg (face-foreground 'default nil t))
           (current-bg (or ;; (face-background 'hl-line nil t)
                           (face-background 'highlight nil t)
                           bg))
           (current-fg (or (face-foreground 'highlight nil t)
                           fg))
           (border (or (face-background 'vertical-border nil t)
                       (face-foreground 'vertical-border nil t)
                       (face-background 'mode-line-inactive nil t)
                       bg)))
      (set-face-attribute 'corfu-default nil
                          :background bg
                          :foreground fg)
      (set-face-attribute 'corfu-current nil
                          :background current-bg
                          :foreground current-fg
                          :extend t)
      (set-face-attribute 'corfu-border nil
                          :background border)))

  (defun +eglot-corfu-setup ()
    "Make Corfu react faster in Eglot-managed buffers."
    (setq-local corfu-auto-delay 0.0))

  (defun +corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if completion is expected."
    (when (local-variable-p 'completion-at-point-functions)
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (+corfu-apply-theme)
  (unless (advice-member-p #'+corfu-apply-theme #'enable-theme)
    (advice-add 'enable-theme :after #'+corfu-apply-theme))
  (add-hook 'eglot-managed-mode-hook #'+eglot-corfu-setup)
  (add-hook 'minibuffer-setup-hook #'+corfu-enable-in-minibuffer)
  :bind (:map corfu-map
              ("S-SPC" . corfu-insert-separator)))

(use-package corfu-prescient
  :straight t
  :after (corfu prescient)
  :custom
  (corfu-prescient-completion-styles '(flex orderless basic))
  :config
  (setq corfu-prescient-enable-sorting t)
  (setq corfu-prescient-enable-filtering t)
  (corfu-prescient-mode t))

;; Corfu popup info (documentation popup)
(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :hook (global-corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-popupinfo-max-height 15)
  (corfu-popupinfo-max-width 80)
  :config
  (set-face-attribute 'corfu-popupinfo nil
                      :inherit 'corfu-default))

;; Nerd-icons for corfu
(use-package nerd-icons-corfu
  :straight t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Cape: completion backends (file, dabbrev, etc.)
(use-package cape
  :straight t
  :after corfu
  :config
  (defun +eglot-capf ()
    "Prefer Eglot's CAPF while keeping lightweight local fallbacks."
    (setq-local completion-at-point-functions
                (list #'cape-file
                      #'eglot-completion-at-point
                      #'cape-dabbrev)))
  (add-hook 'eglot-managed-mode-hook #'+eglot-capf)
  ;; Global fallback backends for non-eglot buffers
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev))

(provide 'init-eglot)
