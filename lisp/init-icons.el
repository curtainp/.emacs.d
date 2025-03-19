;;; -*- lexical-binding: t -*-

(use-package nerd-icons
  :straight t)

              
;; Icons for Corfu using `nerd-icons'
(use-package nerd-icons-corfu
  :disabled
  :straight t
  :after corfu
  :if (not (display-graphic-p))
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Use nerd-icons for completion
(use-package nerd-icons-completion
  :straight t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode 1))

(provide 'init-icons)
