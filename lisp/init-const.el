;; init-const.el -*- lexical-binding: t -*-

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
  (string-equal "root" (getenv "USER"))
  "Are we ROOT user?")

(defconst emacs/>=29-p
  (>= emacs-major-version 29)
  "Emacs is 29 or above?")

(defconst emacs/>=30-p
  (>= emacs-major-version 30)
  "Emacs is 30 or above?")

(defconst emacs/>=31-p
  (>= emacs-major-version 31)
  "Emacs is 31 or above?")

(provide 'init-const)
