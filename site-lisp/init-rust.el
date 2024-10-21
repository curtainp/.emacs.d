;; # -*- lexical-binding: t; -*-

(use-package rust-mode
  :mode ("\\.rs\\'" . rustic-mode)
  :init (setq rust-format-on-save t
              rust-mode-treesitter-derive t)
  :config
  ;; HACK: `global-treesit-auto-mode' will override `rust-mode'.
  (define-derived-mode rustic-mode rust-mode "Rust"
    "Major mode for Rust code.

\\{rust-mode-map}")

  (when (centaur-treesit-available-p)
    (setq auto-mode-alist (delete '("\\.rs\\'" . rust-mode) auto-mode-alist))
    (setq auto-mode-alist (delete '("\\.rs\\'" . rust-ts-mode) auto-mode-alist))))

(provide 'init-rust)
