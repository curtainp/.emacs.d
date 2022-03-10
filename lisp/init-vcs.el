;;;  init-vcs.el -*- lexical-binding: t -*-

(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (with-eval-after-load 'evil
    (evil-add-hjkl-bindings magit-status-mode-map
      'emacs
      (kbd "l") 'nil
      (kbd "h") 'nil
      (kbd "C-u") 'evil-scroll-up
      (kbd "C-d") 'evil-scroll-down
      (kbd "K") 'magit-discard
      (kbd "s-1") 'magit-jump-to-unstaged
      (kbd "s-2") 'magit-jump-to-untracked)))

(provide 'init-vcs)
