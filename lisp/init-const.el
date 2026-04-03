;;; init-const.el --- Shared constants -*- lexical-binding: t; -*-

;;;; Platform

(defconst sys/linux-p
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/mac-p
  (eq system-type 'darwin)
  "Are we running on a Darwin system?")

(defconst sys/mac-gui-p
  (and (display-graphic-p) sys/mac-p)
  "Are we running under GUI on a Darwin system?")

(defconst sys/linux-gui-p
  (and (display-graphic-p) sys/linux-p)
  "Are we running under GUI on a GNU/Linux system?")

(defconst sys/root-p
  (or (and (fboundp 'user-uid)
           (integerp (user-uid))
           (zerop (user-uid)))
      (string-equal "root" user-login-name))
  "Are we ROOT user?")

;;;; Emacs versions

(defconst emacs/>=29-p
  (>= emacs-major-version 29)
  "Emacs is 29 or above?")

(defconst emacs/>=30-p
  (>= emacs-major-version 30)
  "Emacs is 30 or above?")

(defconst emacs/>=31-p
  (>= emacs-major-version 31)
  "Emacs is 31 or above?")

;;;; Fonts

(defconst cw/emoji-fonts
  '("Apple Color Emoji"
    "Noto Color Emoji"
    "Noto Emoji"
    "Segoe UI Emoji")
  "Candidate emoji fonts ordered by preference.")

(defconst cw/symbol-fonts
  '("Apple Symbols"
    "Segoe UI Symbol"
    "Symbola"
    "Symbol")
  "Candidate symbol fonts ordered by preference.")

(defconst cw/zh-font "LXGW WenKai"
  "Default Chinese font family.")

(defconst cw/default-font "Iosevka Term"
  "Default monospace font family.")

(provide 'init-const)
;;; init-const.el ends here
