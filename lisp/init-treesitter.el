;;;  init-treesitter.el -*- lexical-binding: t -*-

(use-package tree-sitter-langs
  :ensure t)

(use-package tree-sitter
  :config
  (require 'tree-sitter-langs)
  (require 'tree-sitter-hl)
  (require 'tree-sitter-query)
  (require 'tree-sitter-debug)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

  ;;  add elisp tree sitter
  (tree-sitter-load 'elisp "elisp")
  (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))
  (add-to-list 'tree-sitter-major-mode-language-alist '(inferior-emacs-lisp-mode . elisp))
  )

(provide 'init-treesitter)
