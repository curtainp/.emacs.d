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

(setq  initial-frame-alist (quote ((fullscreen . maximized))))
(global-display-line-numbers-mode t)
(global-hl-line-mode t)

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
