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

;; TODO


(provide 'init-language)

