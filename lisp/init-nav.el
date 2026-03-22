;;; -*- lexical-binding: t -*-

(use-package ace-window
  :straight t
  :commands ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package avy
  :straight t
  :commands avy-goto-word-or-subword-1
  :bind (("M-j" . avy-goto-word-or-subword-1)))

(use-package color-rg
  :straight (:host github :repo "manateelazycat/color-rg")
  :custom
  (color-rg-mac-load-path-from-shell nil))

(provide 'init-nav)
