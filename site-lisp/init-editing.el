;; -*- lexical-binding: t; -*-

(use-package sis
  :config
  (setq sis-external-ism "macism"
	sis-english-source "com.apple.keylayout.ABC"
	sis-inline-tighten-head-rule nil
	sis-default-cursor-color "#cf7fa7"
	sis-other-cursor-color "orange")
  (if cs/is-mac
      (sis-ism-lazyman-config
       "com.apple.keylayout.ABC"
       "im.rime.inputmethod.Squirrel.Rime")
    (sis-ism-lazyman-config "1" "2" 'fcitx))
  (sis-global-cursor-color-mode t)
  (sis-global-respect-mode t)
  (sis-global-context-mode t)
  ;; (sis-global-inline-mode t)
  )

(provide 'init-editing)
