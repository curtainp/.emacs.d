;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 80000000
                  gc-cons-percentage 0.1)))

(setq package-enable-at-startup nil
      load-prefer-newer t
      read-process-output-max (* 1024 1024)
      highlight-nonselected-windows nil
      ad-redefinition-action 'accept  ; disable warnings from legacy advice API
      warning-suppress-types '((defvaralias) (lexical-binding)) ; ignore warnings from "existing variables being aliased"
      idle-update-delay 1.0
      inhibit-compacting-font-caches t
      site-run-file nil
      inhibit-splash-screen t
      )

(setq-default cursor-in-non-selected-windows nil)

;; temporarily avoid special handling of files
(defvar me/-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist me/-file-name-handler-alist)))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(push '(undecorated-round . t) default-frame-alist)

(setq tool-bar-mode nil
      scroll-bar-mode nil)
(when (bound-and-true-p tooltip-mode)
  tooltip-mode -1)
(setq use-file-dialog nil
      use-dialog-box nil)
(unless (memq window-system '(mac ns))
  (setq menu-bar-mode nil))
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      initial-major-mode 'fundamental-mode)

