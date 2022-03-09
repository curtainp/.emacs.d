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

;; (add-to-list 'default-frame-alist '(undecorated . t))
