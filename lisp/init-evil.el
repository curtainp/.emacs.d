;;; init-evil.el -*- lexical-binding: t -*-

(use-package evil
  :straight t
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil
        evil-spilt-window-below t
        evil-vspilt-window-right t)
  (evil-mode)
  :config
  ;; insert mode cursor movements
  (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-insert-state-map (kbd "C-f") 'forward-char)
  (define-key evil-insert-state-map (kbd "C-b") 'backward-char)
  )

(use-package evil-collection
  :straight t
  :after evil
  :demand t
  :config
  (evil-collection-init))

(provide 'init-evil)
