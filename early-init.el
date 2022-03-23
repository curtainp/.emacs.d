;;; early-initel -*- lexical-binding: t -*-

(setq ;; defer GC util startup
      gc-cons-threshold most-positive-fixnum
      read-process-output-max (* 1024 1024)
      frame-inhibit-implied-resize t
      load-prefer-newer nil
      inhibit-startup-screen t
      create-lockfiles nil
      ;; since v28
      use-short-answers t
      word-wrap-by-category t
      ;; end
      image-use-external-converter t
      package-enable-at-startup nil
      ;; GnuPG
      epa-pinentry-mode 'loopback)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; Setup straight as package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Straight configs
(setq straight-vc-git-default-clone-depth 1)

;; Use GCMH as GC config.
(straight-use-package 'gcmh)
(require 'gcmh)
(gcmh-mode 1)
