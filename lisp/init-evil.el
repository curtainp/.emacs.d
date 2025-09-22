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
  :custom
  (evil-search-module 'evil-search)
  (evil-echo-state nil) ;; we have state indicator with awesome-tray
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
  (evil-define-key 'normal org-mode-map
    "q" 'quit-window)
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
    "pf" 'project-find-file
    )
  ;; color-rg integration with evil
  (evil-define-key nil space-leader-map
      "sg" 'color-rg-search-input
      "sp" 'color-rg-search-input-in-project
      "sb" 'color-rg-search-input-in-current-file)
    (evil-define-key '(normal visual) 'global
      "g*" 'color-rg-search-symbol)
    (dolist (mode '(color-rg-mode color-rg-search-mode color-rg-switch-to-edit-mode color-rg-switch-to-view-mode))
      (evil-set-initial-state mode 'emacs))

  (evil-define-key '(normal insert) 'global
        (kbd "C-x C-p")  'yank-from-kill-ring) ;; NOTE: original bind with mark-page

  ;; global key bindings and initial mode custom for lsp-bridge
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
                         (kbd "M-s-n") 'lsp-bridge-popup-documentation-scroll-up
                         (kbd "M-s-p") 'lsp-bridge-popup-documentation-scroll-down
                         "gp" 'lsp-bridge-peek
                         )
    (dolist (mode '(lsp-bridge-peek-mode lsp-bridge-ref-mode eaf-mode))
      (evil-set-initial-state mode 'emacs))
    ;; (add-hook 'lsp-bridge-peek-mode-hook 'evil-normalize-keymaps) ;
    ;; (evil-define-key '(normal visual) 'lsp-bridge-peek-keymap
    ;;   "M-j" 'lsp-bridge-peek-list-next-line
    ;;   "M-k" 'lsp-bridge-peek-list-prev-line)
    )
  (with-eval-after-load 'eaf
    (evil-define-key '(normal visual insert) 'global
      (kbd "C-x C-j") 'eaf-open-in-file-manager
      ;; (kbd "C-x p s") 'eaf-open-pyqterminal
      ))
  ;; centaur-tabs
  (with-eval-after-load 'centaur-tabs
    (evil-define-key '(normal visual) 'global
                         "H" 'centaur-tabs-backward
                         "L" 'centaur-tabs-forward))
  )

(use-package evil-collection
  :straight t
  :after evil
  :custom
  (evil-collection-want-find-usages-bindings nil)
  (evil-collection-term-state-and-mode-p nil)
  (evil-collection-want-unimpaired-p nil)
  :config
  (evil-collection-init))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))

(provide 'init-evil)
