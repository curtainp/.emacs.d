;; -*- lexical-binding: t -*-

(use-package rime
  :disabled
  :straight (:type git :host github :repo "DogLooksGood/emacs-rime"
                   :files ("*.el" "Makefile" "lib.c"))
  :defer 3
  :custom
  (default-input-method "rime")
  :config
  (setq rime-user-data-dir (expand-file-name "~/.local/share/fcitx5/rime")
        rime-show-candidate 'minibuffer)
  (when (eq system-type 'darwin)
    (setq rime-librime-root "/opt/homebrew"))
  )

(use-package sis
  :straight t
  :config
  (if (eq system-type 'darwin)
      (sis-ism-lazyman-config
       "com.apple.keylayout.ABC"
       "im.rime.inputmethod.Squirrel.Rime")
    (sis-ism-lazyman-config "1" "2" 'fcitx5))
  (sis-global-cursor-color-mode t)
  (sis-global-respect-mode t)
  (sis-global-context-mode t)
  (sis-global-inline-mode t))

(provide 'init-rime)
