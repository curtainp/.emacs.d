;;; init-ui.el -*- lexical-binding: t -*-

(setq frame-title-format '("Curtain Emacs - %b")
      icon-title-format frame-title-format)

(when (and (display-graphic-p) (eq system-type 'darwin))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg)))))

;; Optimization
(setq idle-update-delay 1.0)

(use-package doom-themes
  :custom-face
  (doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
  :init
  (load-theme 'doom-one t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-minor-mode t)
  (unless after-init-time
    (setq-default mode-line-format nil)))

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
  :init
  (global-hl-line-mode 1))

(set-face-attribute 'default nil :height 160 :family "JetBrainsMono Nerd Font Mono")
(let ((emacs-font-size 16)
      emacs-font-name)
         (cond
          ((featurep 'cocoa)
           (setq emacs-font-name "Operator Mono Lig"))
          ((string-equal system-type "gnu/linux")
           (setq emacs-font-name "JetBrainsMono Nerd Font Mono")))
         (when (display-grayscale-p)
           (set-frame-font (format "%s-%s" (eval emacs-font-name) (eval emacs-font-size)))
           (set-fontset-font (frame-parameter nil 'font) 'unicode (eval emacs-font-name))
           ))

(provide 'init-ui)
