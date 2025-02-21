;;; -*- lexical-binding: t -*-

(use-package nerd-icons
  :straight t)

(use-package nerd-icons-dired
  :straight t
  :when (display-graphic-p)
  :hook (dired-mode . nerd-icons-dired-mode))
              
;; Icons for Corfu using `nerd-icons'
;; (use-package nerd-icons-corfu
;;   :straight t
;;   :after corfu
;;   :init
;;   (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Use nerd-icons for completion
(use-package nerd-icons-completion
  :straight t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode 1))

(provide 'init-icons)
