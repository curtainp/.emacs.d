;;; init-ui.el -*- lexical-binding: t -*-


(setq  initial-frame-alist (quote ((fullscreen . maximized))))
(global-display-line-numbers-mode t)
(global-hl-line-mode t)

(set-face-attribute 'default nil :height 150 :family "JetBrainsMono Nerd Font Mono")
(let ((emacs-font-size 14)
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
