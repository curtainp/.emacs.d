;;; init-grease.el -*- lexical-binding: t -*-

(use-package grease
  :straight (:host github :repo "mwac-dev/grease.el")
  :commands (grease-open grease-toggle grease-here)
  :init
  (setq grease-use-icons t)
  (setq grease-sort-directories-first t)
  (setq grease-show-hidden nil)
  (setq grease-preview-window-width 0.4)
  (setq grease-preview-writable nil)
  )

(provide 'init-grease)
