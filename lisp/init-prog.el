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

(provide 'init-prog)
