;;; -*- lexical-binding: t -*-

(setq completions-format 'one-column)
(setq completion-show-help nil)
(setq completion-auto-help 'always)
(setq completion-auto-select nil)
(setq completions-detailed t)
(setq completion-show-inline-help nil)
(setq completions-max-height 6)
(setq completions-header-format (propertize "%s candidates:\n" 'face 'bold-italic))
(setq completions-highlight-face 'completions-highlight)
(setq minibuffer-completion-auto-choose t)
(setq minibuffer-visible-completions t) ; Emacs 30
(setq completions-sort 'historical)

;;;; Completion styles
(setq completion-styles '(basic substring initials flex orderless)) ; also see `completion-category-overrides'
(setq completion-pcm-leading-wildcard t) ; Emacs 31: make `partial-completion' behave like `substring'

;; Reset all the per-category defaults so that (i) we use the
;; standard `completion-styles' and (ii) can specify our own styles
;; in the `completion-category-overrides' without having to
;; explicitly override everything.
(setq completion-category-defaults nil)

(setq completion-category-overrides
      ;; NOTE 2021-10-25: I am adding `basic' because it works better as a
      ;; default for some contexts.  Read:
      ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50387>.
      ;;
      ;; `partial-completion' is a killer app for files, because it
      ;; can expand ~/.l/s/fo to ~/.local/share/fonts.
      ;;
      ;; If `basic' cannot match my current input, Emacs tries the
      ;; next completion style in the given order.  In other words,
      ;; `orderless' kicks in as soon as I input a space or one of its
      ;; style dispatcher characters.
      '((file (styles . (basic partial-completion orderless)))
        (bookmark (styles . (basic substring)))
        (library (styles . (basic substring)))
        (embark-keybinding (styles . (basic substring)))
        (imenu (styles . (basic substring orderless)))
        (consult-location (styles . (basic substring orderless)))
        (kill-ring (styles . (emacs22 orderless)))
        (eglot (styles . (emacs22 substring orderless)))))

(use-package vertico
  :straight t
  :hook (after-init . vertico-mode)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind (("M-R" . vertico-repeat)
         :map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)
         ("M-h" . vertico-directory-up))
  :custom
  (vertico-scroll-margin 0)
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-count 5))

(use-package orderless
  :straight t
  :after minibuffer
  :custom
  (orderless-matching-styles '(orderless-prefixes orderless-regexp))

  ;; SPC should never complete: use it for `orderless' groups.
  ;; The `?' is a regexp construct.
  :bind ( :map minibuffer-local-completion-map
          ("SPC" . nil)
          ("?" . nil)))

(use-package curt-orderless
  :straight nil
  :demand t
  :config
  (setq orderless-style-dispatchers
        '(curt-orderless-literal
          curt-orderless-file-ext
          curt-orderless-beg-or-end)))

(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq-default case-fold-search t)   ; For general regexp
(setq read-file-name-completion-ignore-case t)

(use-package mb-depth
  :straight nil
  :commands minibuffer-depth-indicate-mode
  :hook (after-init . minibuffer-depth-indicate-mode)
  :config
  (setq read-minibuffer-restore-windows nil) ; Emacs 28
  (setq enable-recursive-minibuffers t))

(use-package minibuf-eldef
  :straight nil
  :commands minibuffer-electric-default-mode
  :hook (after-init . minibuffer-electric-default-mode)
  :config
  (setq minibuffer-default-prompt-format " [%s]")) ; Emacs 29

(use-package rfn-eshadow
  :straight nil
  :commands cursor-intangible-mode
  :hook (minibuffer-setup . cursor-intangible-mode)
  :config
  ;; Not everything here comes from rfn-eshadow.el, but this is fine.

  (setq resize-mini-windows t)
  (setq read-answer-short t) ; also check `use-short-answers' for Emacs28
  (setq echo-keystrokes 0.25)
  (setq kill-ring-max 60) ; Keep it small

  ;; Do not allow the cursor to move inside the minibuffer prompt.  I
  ;; got this from the documentation of Daniel Mendler's Vertico
  ;; package: <https://github.com/minad/vertico>.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (file-name-shadow-mode 1))

(use-package marginalia
  :straight t
  :commands (marginalia-mode marginalia-cycle)
  :hook (vertico-mode . marginalia-mode)
  :config
  (setq marginalia-max-relative-age 0)) ; absolute time


(use-package which-key
  :straight t
  :defer 3
  :config
  ;; Show which-key buffer only when C-h or ? trigger
  (setopt which-key-show-early-on-C-h t)
  ;; Only trigger by above key sequence
  (setopt which-key-idle-delay 10000.0)
  ;; Real-time react after which-key buffer shows
  (setopt which-key-idle-secondary-delay 0.05)
  (which-key-mode))


(provide 'init-completion)
