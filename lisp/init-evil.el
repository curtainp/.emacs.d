;;; init-evil.el -*- lexical-binding: t -*-

(use-package evil
  :straight t
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil
        evil-undo-system 'undo-redo
        evil-spilt-window-below t
        evil-symbol-word-search t
        evil-vspilt-window-right t)
  :config
  ;; insert mode cursor movements
  (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-insert-state-map (kbd "C-f") 'forward-char)
  (define-key evil-insert-state-map (kbd "C-b") 'backward-char)
  ;; minibuffer-local-map settings
  (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)
  (define-key minibuffer-local-map (kbd "C-u") 'evil-delete-back-to-indentation)
  (define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
  ;; define space-leader-map
  (define-prefix-command 'space-leader-map)
  (keymap-set evil-motion-state-map "SPC" 'space-leader-map)
  (keymap-set evil-normal-state-map "SPC" 'space-leader-map)

  (evil-define-key nil space-leader-map
    (kbd "SPC") 'execute-extended-command
    (kbd "RET") 'consult-bookmark
    "u" 'universal-argument
    "bb" 'consult-buffer
    "bd" 'evil-delete-buffer
    "br" 'revert-buffer
    "ff" 'find-file
    "fr" 'recentf
    "ss" 'consult-line
    "sg" 'consult-ripgrep
    "pf" 'project-find-file
    )
  ;; global key bindings for lsp-bridge
  (with-eval-after-load 'lsp-bridge
    (evil-define-key '(normal visual) 'global
                         "ga" 'lsp-bridge-code-action
                         "gd" 'lsp-bridge-find-def
                         "gr" 'lsp-bridge-find-references
                         "gR" 'lsp-bridge-rename
                         "[d" 'lsp-bridge-diagnostic-jump-prev
                         "]d" 'lsp-bridge-diagnostic-jump-next
                         "gi" 'lsp-bridge-find-impl
                         "gI" 'lsp-bridge-find-impl-other-window
                         "K"  'lsp-bridge-popup-documentation
                         "gp" 'lsp-bridge-peek
                         ))
  )

(use-package evil-collection
  :straight t
  :after evil
  :demand t
  :config
  (dolist (mode '(fundamental-mode))
    (add-to-list 'evil-collection-mode-list mode))
  (evil-collection-init))

(provide 'init-evil)
