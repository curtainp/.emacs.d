;;; init-language.el -*- lexical-binding: t -*-


;; lisp
(use-package lispy
  :ensure t
  :hook (emacs-lisp-mode . lispy-mode))
(use-package lispyville
  :ensure t
  :hook (lispy-mode . lispyville-mode))
(use-package macrostep
  :ensure t)

;; yaml
(use-package yaml-mode)

;; rust
(use-package rust-mode
  :hook (rust-mode . ada/rust-compile)
  :config
  (setq rust-format-on-save t)
  (defun ada/rust-compile ()
    (setq-local company-backends '(company-capf company-dabbrev-code)
                compile-command "cargo check --color never --tests")))

(use-package cargo
  :hook ((rust-mode . cargo-minor-mode))
  :config
  (defun ada/cargo-test-current ()
    (interactive)
    (setenv "RUST_LOG" "debug")
    (cargo-process-current-test))
  :bind (:map rust-mode-map
         (("C-c C-t" . ada/cargo-test-current)))
  :custom ((cargo-process--command-current-test "test --color never")
           (cargo-process--enable-rust-backtrace t)
           (cargo-process--command-flags "--  --nocapture")))



(provide 'init-language)

