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
  :disabled
  :straight (:host github :repo "manateelazycat/color-rg")
  :defer t
  :custom
  (color-rg-mac-load-path-from-shell nil))

(use-package citre
  :straight t
  :bind (:map prog-mode-map
              ("C-c c j" . cw/citre-jump)
              ("C-c c k" . cw/citre-jump-back)
              ("C-c c p" . citre-peek)
              ("C-c c a" . citre-ace-peek)
              ("C-c c u" . citre-update-this-tags-file))
  :init
  (require 'citre-config)
  :config

  (setq citre-auto-enable-citre-mode-modes '(prog-mode)
        citre-default-create-tags-file-location 'global-cache
        citre-edit-ctags-options-manually t
        citre-enable-capf-integration t)
  (defun cw/citre-jump ()
    "Jump to the definition of the symbol at point, fallback to `xref-find-definitions'."
    (interactive)
    (condition-case _
        (citre-jump)
      (error (call-interactively #'xref-find-definitions))))
  (defun cw/citre-jump-back ()
    "Go back to the position before last `citre-jump', fallback to `xref-go-back'."
    (interactive)
    (condition-case _
        (citre-jump-back)
      (error (call-interactively #'xref-go-back)))))


(provide 'init-nav)
