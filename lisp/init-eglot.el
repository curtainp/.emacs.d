;;; init-eglot.el -*- lexical-binding: t -*-

;; Eglot (built-in since Emacs 29)
(use-package eglot
  :straight nil
  :commands (eglot eglot-ensure)
  :hook ((python-mode
          python-ts-mode
          rust-mode
          rust-ts-mode
          zig-mode
          c-ts-mode c++-ts-mode
          c-mode c++-mode)
         . eglot-ensure)
  :bind
  (:map eglot-mode-map
        ("C-c e i" . eglot-find-implementation)
        ("C-c e a" . eglot-code-actions)
        ("C-c e r" . eglot-rename))
  :config
  (setq eglot-autoshutdown t
        eglot-events-buffer-config '(:size 0 :format full) ;; no log
        ;; Keep the server closer to the live buffer so completion
        ;; doesn't lag behind fast typing.
        eglot-send-changes-idle-time 0.05
        eglot-code-action-indications nil
        ;; format with `apheleia-format-buffer' instead.
        eglot-ignored-server-capabilities '(:documentFormattingProvider
                                            :documentRangeFormattingProvider)
        eglot-report-progress 'messages)
  (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (setq-default eglot-workspace-configuration
                '(
                  (:rust-analyzer . (:cargo (:allFeatures t :allTargets t :features "full")
                                            :checkOnSave :json-false
                                            :completion (:termSearch (:enable t)
                                                                     :fullFunctionSignatures (:enable t))
                                            :hover (:memoryLayout (:size "both")
                                                                  :show (:traitAssocItems 5)
                                                                  :documentation (:keywords (:enable :json-false)))
                                            :inlayHints(;:bindingModeHints (:enable t)
                                                        :lifetimeElisionHints (:enable "skip_trivial" :useParameterNames t)
                                                        :closureReturnTypeHints (:enable "always")
                                                        :discriminantHints (:enable t)
                                                        :genericParameterHints (:lifetime (:enable t)))
                                            :semanticHighlighting (:operator (:specialization (:enable t))
                                                                             :punctuation (:enable t :specialization (:enable t)))
                                            :workspace (:symbol (:search (:kind "all_symbols"
                                                                                :scope "workspace_and_dependencies")))
                                            :lru (:capacity 1024)
                                            :diagnostics (:enable :json-false)))
                  )))

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
  :straight (:files (:defaults "extensions/*.el"))
  :commands (global-corfu-mode corfu-mode)
  :hook ((prog-mode conf-mode yaml-mode text-mode) . corfu-mode)
  :bind (:map corfu-map
              ("TAB" . corfu-complete)
              ("<tab>" . corfu-complete)
              ("RET" . nil))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  ;; (corfu-preselect 'prompt)
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

  (defun +corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if completion is expected."
    (when (local-variable-p 'completion-at-point-functions)
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (+corfu-apply-theme)
  (unless (advice-member-p #'+corfu-apply-theme #'enable-theme)
    (advice-add 'enable-theme :after #'+corfu-apply-theme))
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
  (setq corfu-prescient-enable-filtering nil)
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
  :disabled
  :straight t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Cape: completion backends (file, dabbrev, etc.)
(use-package cape
  :straight t
  :hook (((prog-mode conf-mode yaml-mode shell-mode eshell-mode text-mode codex-ide-session-mode) . +completion-add-default-capfs)
         ((TeX-mode LaTeX-mode org-mode markdown-mode) . +completion-add-tex-capfs))
  :init
  (defun +completion-add-capfs (&rest capfs)
    "Append CAPFS to the buffer-local `completion-at-point-functions'."
    (dolist (capf capfs)
      (unless (memq capf completion-at-point-functions)
        (setq-local completion-at-point-functions
                    (append completion-at-point-functions (list capf))))))

  (defun +completion-add-default-capfs ()
    (+completion-add-capfs #'cape-file #'cape-dabbrev))

  (defun +completion-add-tex-capfs ()
    (+completion-add-capfs #'cape-tex)))

(provide 'init-eglot)
