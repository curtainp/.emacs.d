;;; -*- lexical-binding: t -*-

(use-package minibuffer
  :straight nil
  :config
  ;;;; Completion styles
  (setq completion-styles '(basic substring initials flex orderless)) ; also see `completion-category-overrides'
  (setq completion-pcm-leading-wildcard t) ; Emacs 31: make `partial-completion' behave like `substring'

  ;; Reset all the per-category defaults so that (i) we use the
  ;; standard `completion-styles' and (ii) can specify our own styles
  ;; in the `completion-category-overrides' without having to
  ;; explicitly override everything.
  (setq completion-category-defaults nil)
  
  ;; A non-exhaustve list of known completion categories:
  ;;
  ;; - `bookmark'
  ;; - `buffer'
  ;; - `charset'
  ;; - `coding-system'
  ;; - `color'
  ;; - `command' (e.g. `M-x')
  ;; - `customize-group'
  ;; - `environment-variable'
  ;; - `expression'
  ;; - `face'
  ;; - `file'
  ;; - `function' (the `describe-function' command bound to `C-h f')
  ;; - `info-menu'
  ;; - `imenu'
  ;; - `input-method'
  ;; - `kill-ring'
  ;; - `library'
  ;; - `minor-mode'
  ;; - `multi-category'
  ;; - `package'
  ;; - `project-file'
  ;; - `symbol' (the `describe-symbol' command bound to `C-h o')
  ;; - `theme'
  ;; - `unicode-name' (the `insert-char' command bound to `C-x 8 RET')
  ;; - `variable' (the `describe-variable' command bound to `C-h v')
  ;; - `consult-grep'
  ;; - `consult-isearch'
  ;; - `consult-kmacro'
  ;; - `consult-location'
  ;; - `embark-keybinding'
  ;;
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
          (eglot (styles . (emacs22 substring orderless))))))


(use-package orderless
  :straight t
  :demand t
  :after minibuffer
  :config
  ;; Remember to check my `completion-styles' and the
  ;; `completion-category-overrides'.
  (setq orderless-matching-styles '(orderless-prefixes orderless-regexp))

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
  :hook (after-init . minibuffer-depth-indicate-mode)
  :config
  (setq read-minibuffer-restore-windows nil) ; Emacs 28
  (setq enable-recursive-minibuffers t))

(use-package minibuf-eldef
  :straight nil
  :hook (after-init . minibuffer-electric-default-mode)
  :config
  (setq minibuffer-default-prompt-format " [%s]")) ; Emacs 29

(use-package rfn-eshadow
  :straight nil
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

(use-package minibuffer
  :straight nil
  :config
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
  )

(use-package savehist
  :straight nil
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 100)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-to-list 'savehist-additional-variables 'kill-ring))

;; Corfu enhances in-buffer completion with a small completion popup
(use-package corfu
  :disabled
  :straight (:files (:defaults "extensions/*.el"))
  :hook ((eshell-mode shell-mode) . +corfu-less-intrusive-h)
  :hook (minibuffer-setup . +corfu-enable-in-minibuffer-h)
  :hook (corfu-mode . corfu-history-mode)
  :if (not (display-graphic-p))
  :custom
  (corfu-auto t) ; Enable auto completion
  (corfu-cycle t) ; Allows cycling through candidates
  (corfu-min-width 25)
  (corfu-preview-current nil) ; Disable previewing the current candidate
  :init
  (add-hook 'prog-mode-hook #'global-corfu-mode)
  :config
  ;; HACK: Prevent the annoting completion error when no `ispell' dictionary is set, prefer `cape-dict'
  (when (>= emacs-major-version 30)
    (setq text-mode-ispell-word-completion nil))

  (defun +corfu-enable-in-minibuffer-h ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (local-variable-p 'completion-at-point-functions)
      (setq-local corfu-auto t ; Enable/disable auto completion
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (defun +corfu-less-intrusive-h ()
    (setq-local corfu-quit-at-boundary t
                corfu-quit-no-match t
                corfu-auto nil)
    (corfu-mode 1))

  ;; Ensure `savehist-mode' is on and add `corfu-history' to the saved variables
  (unless (bound-and-true-p savehist-mode) (savehist-mode 1))
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; Candidate information popup for Corfu
(use-package corfu-popupinfo
  :disabled
  :if (not (display-graphic-p))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :bind ( ; Bind these to toggle/scroll documentation
         :map corfu-map
         ("M-p" . corfu-popupinfo-scroll-down)
         ("M-n" . corfu-popupinfo-scroll-up)
         ("M-d" . corfu-popupinfo-toggle))
  :custom
  (corfu-popupinfo-delay nil)
  (corfu-popupinfo-max-height 15)
  :config
  ;; Otherwise, the popupinfo will stay open on ESC or `C-g'!
  (add-hook
   'completion-in-region-mode-hook
   (defun +corfu--hide-popupinfo-h ()
     (when (and (not completion-in-region-mode) (boundp 'corfu-popupinfo--hide))
       (corfu-popupinfo--hide)))))

(use-package consult
  :disabled
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  ( :map global-map
    ("M-K" . consult-keep-lines) ; M-S-k is similar to M-S-5 (M-%)
    ("M-F" . consult-focus-lines) ; same principle
    ("M-s b" . consult-buffer)
    ("M-s f" . consult-find)
    ("M-s r" . consult-ripgrep)
    ("M-s h" . consult-history)
    ("M-s i" . consult-imenu)
    ("M-s l" . consult-line)
    ("M-s m" . consult-mark)
    ("M-s y" . consult-yank-pop)
    ("M-s s" . consult-outline)
    :map consult-narrow-map
    ("?" . consult-narrow-help))
  :config
  (setq consult-line-numbers-widen t)
  ;; (setq completion-in-region-function #'consult-completion-in-region)
  (setq consult-async-min-input 3)
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (setq consult-narrow-key nil)
  (setq consult-find-args
        (concat "find . -not ( "
                "-path */.git* -prune "
                "-or -path */.cache* -prune )"))
  (setq consult-preview-key 'any)
  (setq consult-project-function nil) ; always work from the current directory (use `cd' to switch directory)

  (add-to-list 'consult-mode-histories '(vc-git-log-edit-mode . log-edit-comment-ring))

  (require 'consult-imenu) ; the `imenu' extension is in its own file
  )

(use-package embark
  :disabled
  :defer 1
  :config
  (setq embark-confirm-act-all nil)
  (setq embark-mixed-indicator-both nil)
  (setq embark-mixed-indicator-delay 1.0)
  (setq embark-indicators '(embark-mixed-indicator embark-highlight-indicator))
  (setq embark-verbose-indicator-nested nil) ; I think I don't have them, but I do not want them either
  (setq embark-verbose-indicator-buffer-sections '(bindings))
  (setq embark-verbose-indicator-excluded-actions
        '(embark-cycle embark-act-all embark-collect embark-export embark-insert))

  ;; I never cycle and want to disable the damn thing.  Normally, a
  ;; nil value disables a key binding but here that value is
  ;; interpreted as the binding for `embark-act'.  So I just add
  ;; some obscure key that I do not have.  I absolutely do not want
  ;; to cycle!
  (setq embark-cycle-key "<XF86Travel>")
  )

(use-package curt-embark
  :disabled
  :straight nil
  :after embark
  :bind
  ( :map global-map
    ("C-," . curt-embark-act-no-quit)
    ("C-." . curt-embark-act-quit)
    :map embark-collect-mode-map
    ("C-," . curt-embark-act-no-quit)
    ("C-." . curt-embark-act-quit)
    :map minibuffer-local-filename-completion-map
    ("C-," . curt-embark-act-no-quit)
    ("C-." . curt-embark-act-quit))
  :config
  (setq embark-keymap-alist
        '((buffer curt-embark-buffer-map)
          (command curt-embark-command-map)
          (expression curt-embark-expression-map)
          (file curt-embark-file-map)
          (function curt-embark-function-map)
          (identifier curt-embark-identifier-map)
          (package curt-embark-package-map)
          (region curt-embark-region-map)
          (symbol curt-embark-symbol-map)
          (url curt-embark-url-map)
          (variable curt-embark-variable-map)
          (t embark-general-map))))

(use-package embark-consult
  :disabled
  :after (embark consult))

(use-package marginalia
  :straight t
  :hook (vertico-mode . marginalia-mode)
  :config
  (setq marginalia-max-relative-age 0)) ; absolute time

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
