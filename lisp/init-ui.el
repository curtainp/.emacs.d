;;; init-ui.el -*- lexical-binding: t -*-

(use-package dracula-theme
  :ensure t)

(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'tango t))
    ('dark (load-theme 'dracula t))))
(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

;; (global-display-line-numbers-mode t)
(use-package display-line-numbers
  :defer t
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode))

(use-package time
  :defer 5
  :custom ((display-time-format "[%H:%M, %a]")
           (display-time-use-mail-icon t)
           (display-time-default-load-average nil))
  :config
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html
  (display-time-mode 1))

;; (global-hl-line-mode t)
(use-package hl-line
  :config
  (global-hl-line-mode 1))

(set-face-attribute 'default nil :height 160 :family "JetBrainsMono Nerd Font Mono")
(let ((emacs-font-size 15)
      emacs-font-name)
         (cond
          ((featurep 'cocoa)
           (setq emacs-font-name "JetBrainsMono Nerd Font Mono"))
          ((string-equal system-type "gnu/linux")
           (setq emacs-font-name "JetBrainsMono Nerd Font Mono")))
         (when (display-grayscale-p)
           (set-frame-font (format "%s-%s" (eval emacs-font-name) (eval emacs-font-size)))
           (set-fontset-font (frame-parameter nil 'font) 'unicode (eval emacs-font-name))
           ))

(provide 'init-ui)
