;; -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)
;;(setq frame-inhibit-implied-resize t)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 80000000
                  gc-cons-percentage 0.1)))

;; temporarily avoid special handling of files
(defvar me/-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist me/-file-name-handler-alist)))

(setq site-run-file nil)

(setq inhibit-compacting-font-caches t)

(when (boundp 'read-process-output-max)
  ;; default 4096 bytes, set to 1MB
  (setq read-process-output-max 1048576))

(add-to-list 'default-frame-alist '(undecorated-round . t))
(add-to-list 'default-frame-alist `(font . "IosevkaTerm Nerd Font-18"))
(setq-default inhibit-splash-screen t)
(setq frame-resize-pixelwise t)
