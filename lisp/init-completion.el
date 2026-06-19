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
;; (setq text-mode-ispell-word-completion nil) ;; disable ispell-completion

;;;; Completion styles
(setq completion-styles '(orderless basic)) ; also see `completion-category-overrides'
(setq completion-flex-nospace t)
(setq completion-pcm-leading-wildcard t) ; Emacs 31: make `partial-completion' behave like `substring'

;; Reset all the per-category defaults so that (i) we use the
;; standard `completion-styles' and (ii) can specify our own styles
;; in the `completion-category-overrides' without having to
;; explicitly override everything.
(setq completion-category-defaults nil)

(let* ((eager-update-properties '((eager-display . nil)
                                  (eager-update . t)))
       (eager-update-properties-no-sort (append eager-update-properties (list (cons 'display-sort-function #'identity)))))
  (setq completion-category-overrides
        `((file ((styles . (partial-completion))
                 (eager-display . nil)
                 (eager-update . t)))
          (bookmark (,@eager-update-properties))
          (project-file . (,@eager-update-properties))
          (symbol-help . (,@eager-update-properties))
          (buffer . (,@eager-update-properties))
          (command . (affixation-function . nil))
          (denote-file . ,eager-update-properties)
          (theme . ,eager-update-properties)
          (unicode-name . ,eager-update-properties)
          (imenu . ,eager-update-properties-no-sort)
          (consult-location . ,eager-update-properties-no-sort)
          (kill-ring (styles . (emacs22 orderless)))
          (eglot (styles orderless))
          )))

(use-package vertico
  :straight (:files (:defaults "extensions/*.el"))
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
(setq minibuffer-history-case-insensitive-variables t)

(use-package prescient
  :straight t
  :after orderless
  :config
  (prescient-persist-mode t))

(use-package vertico-prescient
  :straight t
  :after (prescient vertico)
  :custom
  (vertico-prescient-completion-styles '(flex orderless))
  :config
  (setq vertico-prescient-enable-sorting t)
  (setq vertico-prescient-enable-filtering nil)
  (vertico-prescient-mode t))

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
  (setq crm-prompt (format "%s %%p" (propertize "[%d]" 'face 'shadow))) ; Emacs 31
  (file-name-shadow-mode 1))

(use-package marginalia
  :straight t
  :commands (marginalia-mode marginalia-cycle)
  :hook (vertico-mode . marginalia-mode)
  :config
  (setq marginalia-max-relative-age 0)) ; absolute time

(use-package consult
  :straight nil
  :bind (
         ("C-c M-x" . consult-mode-command)
         ("C-c h"   . consult-history)
         ("C-c k"   . consult-kmacro)
         ("C-c i"   . consult-info)
         ("C-c /"   . consult-ripgrep)
         ;; registers
         ("M-#"     . consult-register-load)
         ("M-'"     . consult-register-store)
         ("C-M-#"   . consult-register)
         ;; yank
         ("M-y"     . consult-yank-pop)
         ;; goto-map
         ("M-g g"   . consult-goto-line)
         ("M-g o"   . consult-outline)
         ("M-g m"   . consult-mark)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)
         ;; search-map
         ("M-s c"   . consult-locate)
         ("M-s g"   . consult-ripgrep)
         ("M-s f"   . consult-fd)
         ("M-s l"   . consult-line)
              )
  :config
  (setq consult-line-numbers-widen t)
  (setq consult-async-min-input 3)
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (setq consult-narrow-key nil)
  (setq consult-ripgrep-args
        "rg --hidden --glob \"!.git\" --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number")
  (setq consult-find-args
        (concat "find . -not ( ":
                "-path */.git* -prune "
                "-or -path */.cache* -prune )"))
  (setq consult-preview-key 'any)
  (setq consult-project-function nil)
  (setq consult-after-jump-hook nil)    ; use `pulsar' instead.
  (add-hook 'consult-after-jump-hook (lambda ()
                                       (pulsar-recenter-top)
                                       (pulsar-reveal-entry)))
  )

(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :straight t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
              ("C-c C-c" . embark-collect)
              ("C-c C-e" . embark-export)))

(use-package embark-consult
  :straight t
  :after (embark consult))

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
