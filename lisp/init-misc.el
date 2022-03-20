;;;  init-misc.el -*- lexical-binding: t -*-

(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package rime
  :defer t
  :custom
  ((default-input-method "rime")
   (rime-librime-root (expand-file-name "site-lisp/librime/dist" user-emacs-directory))
   (rime-user-data-dir (expand-file-name "site-lisp/librime/rime-config" user-emacs-directory))
   (rime-show-candidate 'posframe)
   (rime-popup-style 'vertical)
   (rime-posframe-style 'vertical))
  :bind
  (("<s-return>" . toggle-input-method)
   :map rime-active-mode-map
   ("M-j" . rime-inline-acsii)
   :map rime-mode-map
   ("C-`" . rime-send-keybinding)
   ("C-*" . rime-send-keybinding)
   ("C-&" . rime-send-keybinding)
   ("C-." . rime-send-keybinding)
   ("M-j" . rime-force-enable))
  :config
  (setq mode-line-mule-info '((:eval (rime-lighter))))
  :init
  (progn
    (defun my/toggle-rime ()
      (interactive)
      (when rime-mode
        (rime-lib-process-key 42 4)))
    (setq rime-disable-predicates
          `(rime-predicate-evil-mode-p
            rime-predicate-prog-in-code-p))))

(provide 'init-misc)
