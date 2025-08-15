;;; -*- lexical-binding: t  -*-
;;; Mainly for speeding up startup time

;; Better garbage collection settings, no GCMH required, See: https://zenodo.org/records/10518083
(setq gc-cons-threshold (* 100 1000 1000)
      gc-cons-percentage 0.2
      package-enable-at-startup nil
      ;; `use-package' is built-in from 29, so we need set it before loading `use-package'
      use-package-enable-imenu-support t
      load-prefer-newer t
      default-frame-alist '((tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (vertical-scroll-bars)
                            (undecorated-round . t)
                            (internal-border-width . 0)
                            (fullscreen . maximized))
      tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil
      ;; prevent modeline from showing at startup.
      mode-line-format nil
      ;; suppress don't have lexical-binding warnings
      warning-suppress-types '((files))
      )
(prefer-coding-system 'utf-8)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(setq frame-inhibit-implied-resize t)

;; reduce rendering scan work for non-focused window
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; disable warnings from the legacy advice API
(setq ad-redefinition-action 'accept)

;; FIXME: Emacs updates its ui more often that it needs to ?
(setq idle-update-delay 1.0)
(setq inhibit-compacting-font-caches t)
;; Disable [bidirectional text] scanning for a modest performance
;; Will improve long line display performance
(setq bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right
              bidi-display-reordering 'left-to-right)

;; startup screen
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t
      inhibit-x-resources t
      inhibit-default-init t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)
(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)
(setq use-file-dialog nil
      use-dialog-box nil)

(setq-default inhibit-redisplay t
	      inhibit-message t)

(add-hook 'window-setup-hook
	  (lambda ()
	    (setq-default inhibit-redisplay nil
			  inhibit-message nil)
	    (redraw-frame)))

(setq auto-mode-case-fold nil)

;; `file-name-handler-alist' is consulted on each call to `require', `load', or various file/io functions
(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-value)))))))
