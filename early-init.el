;;; -*- no-byte-compile: t; lexical-binding: t;  -*-

(setq package-enable-at-startup nil)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024))) 105)
                                        ; make gc-restore at the end of `emacs-startup-hook'

;; Prefer loading newer compiled files
(setq load-prefer-newer t)
(setq frame-inhibit-implied-resize t)

;; reduce rendering scan work for non-focused window
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; disable warnings from the legacy advice API
(setq ad-redefinition-action 'accept)

;; ignore warnings about "existing variables being aliased"
(setq warning-suppress-types '((defvaralias) (lexical-binding)))

;; don't ping things that look like domain names
(setq ffap-machine-p-known 'reject)

(setq idle-update-delay 1.0)
(setq inhibit-compacting-font-caches t)
;; Disable [bidirectional text] scanning for a modest performance
;; Will improve long line display performance
(setq bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right
              bidi-display-reordering 'left-to-right)

;; Don't want a mode line while loading init
(setq-default mode-line-format nil)

;; No alarms by default
(setq ring-bell-function 'ignore)

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

;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq-default inhibit-redisplay t
	      inhibit-message t)

(add-hook 'window-setup-hook
	  (lambda ()
	    (setq-default inhibit-redisplay nil
			  inhibit-message nil)
	    (redraw-frame)))

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(push '(undecorated-round . t) default-frame-alist)
(setq auto-mode-case-fold nil)

(setq tool-bar-mode nil
      scroll-bar-mode nil)

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
