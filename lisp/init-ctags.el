;;; init-ctags.el -*- lexical-binding: t -*-

(use-package citre
  :ensure nil
  :hook (citre-mode . ada/citre-hook)
  :commands (citre-mode citre-create-tags-file)
  :init
  (defun ada/citre-hook ()
    (require 'citre)
    (with-eval-after-load 'cc-mode (require 'citre-lang-c))
	(evil-make-overriding-map citre-mode-map)
    (evil-make-overriding-map citre-peek-keymap))
  :bind (:map citre-mode-map
		 ("s-p" . citre-peek)
		 ("s-." . citre-jump)
		 ("s-," . citre-jump-back))
  :config
  (setq citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t))

(provide 'init-ctags)
