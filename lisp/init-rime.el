;; -*- lexical-binding: t -*-

(use-package rime
  :straight (:type git :host github :repo "DogLooksGood/emacs-rime"
                   :files ("*.el" "Makefile" "lib.c"))
  :defer 3
  :custom
  (default-input-method "rime")
  :config
  (setq rime-user-data-dir (expand-file-name "~/.config/fcitx/rime")
        rime-show-candidate 'minibuffer)
  (when (eq system-type 'darwin)
    (setq rime-librime-root "/opt/homebrew"))
  )

(provide 'init-rime)
