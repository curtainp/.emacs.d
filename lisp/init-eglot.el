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
          html-mode)
         . eglot-ensure)
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-events-buffer-size 0)
  (setq eglot-send-changes-idle-time 0.5)
  (setq eglot-report-progress nil))

;; Eglot-booster: wraps emacs-lsp-booster for faster JSON processing
;; Requires `emacs-lsp-booster' binary in PATH
(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
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
  (defun +corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if completion is expected."
    (when (local-variable-p 'completion-at-point-functions)
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'+corfu-enable-in-minibuffer)
  :bind (:map corfu-map
              ("S-SPC" . corfu-insert-separator)))

;; Corfu popup info (documentation popup)
(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :hook (global-corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-popupinfo-max-height 15)
  (corfu-popupinfo-max-width 80))

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
    "Compose eglot capf with cape backends."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'cape-file
                       #'cape-dabbrev))))
  (add-hook 'eglot-managed-mode-hook #'+eglot-capf)
  ;; Global fallback backends for non-eglot buffers
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev))

(provide 'init-eglot)
