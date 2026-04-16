;;; -*- lexical-binding: t -*-

(setq backward-delete-char-untabify-method 'hungry)
(setq tab-always-indent 'complete) ; try indent first, if indent already, try complete
(setq tab-first-completion 'word-or-paren-or-punct)
(setq-default tab-width 4
              indent-tabs-mode nil)
(setq default-input-method nil)

(setq native-comp-async-query-on-exit t)

(setq read-answer-short t)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(when (and (eq system-type 'darwin) (display-graphic-p))
  ;; NOTE: When PATH is changed, run following command to update:
  ;; sh -c 'printf "%s" "$PATH"' > .env
  (condition-case err
      (let ((path (with-temp-buffer
                    (insert-file-contents-literally "~/.emacs.d/.macos_exec_path")
                    (buffer-string))))
        (setenv "PATH" path)
        (setq exec-path (append (parse-colon-path path) (list exec-directory))))
    (error (warn "%s" (error-message-string err)))))

(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))

(setq which-func-update-delay 1.0)
(setq idle-update-delay which-func-update-delay)

(defalias #'view-hello-file #'ignore)   ;; never show hello file

(setq visible-bell nil)
(setq ring-bell-function #'ignore)

(setq x-underline-at-descent-line t)

(setq truncate-string-ellipsis "…")

;; show-paren
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

(setq custom-buffer-done-kill t)

(setq next-line-add-newlines nil)

(setq uniquify-buffer-name-style 'forward)

(setq remote-file-name-inhibit-cache 50)

(setq redisplay-skip-fontification-on-input t)

(setq eval-expression-print-level nil
      eval-expression-print-length nil)

(setq auth-sources (list "~/.authinfo.gpg"))

(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)

;; Automatically rescan the buffer for Imenu entries when `imenu' is invoked
;; This ensures the index reflects recent edits.
(setq imenu-auto-rescan t)

;; Prevent truncation of long function names in `imenu' listings
(setq imenu-max-item-length 160)
(setq delete-by-moving-to-trash (not noninteractive))
(setq remote-file-name-inhibit-delete-by-moving-to-trash t)
;;; Tramp
(setq tramp-verbose 1)
;; Ignoring this is acceptable since it will redirect to the buffer regardless.
(setq find-file-suppress-same-file-warnings t)

;; Resolve symlinks to avoid duplicate buffers
(setq find-file-visit-truename t
      ;; Automatically follow a symlink to its source if that source is managed
      ;; by a version control system, rather than asking for permission.
      vc-follow-symlinks t)
(setq create-lockfiles nil)

;; Disable backup files (e.g., filename~). Note that `auto-save-default'
;; remains enabled by default. Even with `make-backup-files' backups disabled,
;; Emacs will still generate temporary recovery files (e.g., #filename#) for
;; unsaved buffers. This protects your active work from sudden crashes while
;; ensuring the file system is cleaned up immediately upon a successful save.
(setq make-backup-files nil)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" user-emacs-directory))))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq backup-by-copying-when-linked t)
(setq backup-by-copying t)  ; Backup by copying rather renaming
(setq delete-old-versions t)  ; Delete excess backup versions silently
(setq version-control t)  ; Use version numbers for backup files
(setq kept-new-versions 5)
(setq kept-old-versions 5)

(setq vc-git-print-log-follow t)
(setq vc-git-diff-switches '("--histogram"))  ; Faster algorithm for diffing.
;; Enable auto-save to safeguard against crashes or data loss. The
;; `recover-file' or `recover-session' functions can be used to restore
;; auto-saved data.
(setq auto-save-no-message t)

(when noninteractive
  ;; The command line interface
  (setq enable-dir-local-variables nil)
  (setq case-fold-search nil))

;; Do not auto-disable auto-save after deleting large chunks of
;; text.
(setq auto-save-include-big-deletions t)

(setq auto-save-list-file-prefix
      (expand-file-name "autosave/" user-emacs-directory))
(setq tramp-auto-save-directory
      (expand-file-name "tramp-autosave/" user-emacs-directory))

(setq auto-save-file-name-transforms
      `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
         ,(file-name-concat auto-save-list-file-prefix "tramp-\\2-") sha1)
        ("\\`/\\([^/]+/\\)*\\([^/]+\\)\\'"
         ,(file-name-concat auto-save-list-file-prefix "\\2-") sha1)))

;; Ensure the directory for auto-save session logs exists with restricted
;; permissions.
(when auto-save-default
  (let ((auto-save-dir (file-name-directory auto-save-list-file-prefix)))
    (unless (file-exists-p auto-save-dir)
      (with-file-modes #o700
        (make-directory auto-save-dir t)))))

;; Auto save options
(setq kill-buffer-delete-auto-save-files t)

;; Remove duplicates from the kill ring to reduce clutter
(setq kill-do-not-save-duplicates t)

;;; recentf

;; `recentf' is an that maintains a list of recently accessed files.
(setq recentf-max-saved-items 300) ; default is 20
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup 'mode)

;; Enables Emacs to remember the last location within a file upon reopening.
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq save-place-limit 600)

;;; savehist

;; `savehist-mode' is an Emacs feature that preserves the minibuffer history
;; between sessions.
(setq history-length 300)
(setq savehist-additional-variables
      '(register-alist                   ; macros
        mark-ring global-mark-ring       ; marks
        search-ring regexp-search-ring)) ; searches

(setq resize-mini-windows 'grow-only)

;; The native border "uses" a pixel of the fringe on the rightmost
;; splits, whereas `window-divider-mode' does not.
(setq window-divider-default-bottom-width 1
      window-divider-default-places t
      window-divider-default-right-width 1)

(setq-default
 ;; smaller threshold to improve long line performance
 long-line-threshold 1000
 large-hscroll-threshold 1000
 syntax-wholeline-max 1000

 column-number-mode t
 ;; Larger process output buffer for LSP module
 read-process-output-max (* 4 1024 1024)
 ;; Wrap words at whitespace, rather than in the middle of a word.
 word-wrap t
 ;; don't do any wrapping by default since it's expensive
 truncate-lines t
 truncate-partial-width-windows nil
 ;; better wrapping for cjk
 word-wrap-by-category t

 ;; Case insensitive completion
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t

 ;; FIX: emacs-plus@31 will cause bug with lsp-bridge acm
 alter-fullscreen-frames nil

 ;; set [fill column] indicator to 100
 fill-column 100

 next-error-recenter '(4)
 find-library-include-other-files nil
 remote-file-name-inhibit-auto-save t
 tramp-connection-timeout (* 60 10)
 duplicate-line-final-position -1
 duplicate-region-final-position -1
 echo-keystrokes-help nil

 ;; indent offset for language
 c-basic-offset 4

 ;; Sentence end
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
 sentence-end-double-space nil

 ;; Inhibit switching out from `y-or-n-p' and `read-char-choice'
 y-or-n-p-use-read-key t
 read-char-choice-use-read-key t

 ;; POSIX standard [newline]
 require-final-newline t

 ;; disable `tramp-mode'
 tramp-mode nil

 ;; Don't prompt for confirmation when creating a new file or buffer
 confirm-nonexistent-file-or-buffer nil

 ;; Shell command
 shell-command-prompt-show-cwd t

 ;; What-cursor-position
 what-cursor-show-names t

 ;; List only applicable commands
 read-extended-command-predicate #'command-completion-default-include-p
 )

;; make underscore as part of the word
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

;; Enables faster scrolling. This may result in brief periods of inaccurate
;; syntax highlighting, which should quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Move point to top/bottom of buffer before signaling a scrolling error.
(setq scroll-error-top-bottom t)

;; Keep screen position if scroll command moved it vertically out of the window.
(setq scroll-preserve-screen-position t)

;; 1. Preventing automatic adjustments to `window-vscroll' for long lines.
;; 2. Resolving the issue of random half-screen jumps during scrolling.
(setq auto-window-vscroll nil)

;; Horizontal scrolling
(setq hscroll-margin 2
      hscroll-step 1)

(setq scroll-conservatively 20)
;; The blinking cursor is distracting and interferes with cursor settings in
;; some minor modes that try to change it buffer-locally (e.g., Treemacs).
(when (bound-and-true-p blink-cursor-mode)
  (blink-cursor-mode -1))
(setq blink-matching-paren nil)

(setq highlight-nonselected-windows nil)
;; Avoid automatic frame resizing when adjusting settings.
(setq global-text-scale-adjust-resizes-frames nil)
(setq delete-pair-blink-delay 0.03)

;; Configure automatic indentation to be triggered exclusively by newline and
;; DEL (backspace) characters.
(setq-default electric-indent-chars '(?\n ?\^?))

(setq comment-multi-line t)
(setq comment-empty-lines t)

(setq sh-indent-after-continuation 'always)
;; Fixes #11: Prevents help command completion from triggering autoload.
;; Loading additional files for completion can slow down help commands and may
;; unintentionally execute initialization code from some libraries.
(setq help-enable-completion-autoload nil)
(setq help-enable-autoload nil)
(setq help-enable-symbol-autoload nil)
(setq help-window-select t)  ;; Focus new help windows when opened

(setq flymake-show-diagnostics-at-end-of-line nil)
(setq flymake-wrap-around nil)

(setq hl-line-sticky-flag nil)
(setq global-hl-line-sticky-flag nil)

(use-package emacs
  :straight nil
  :custom
  (display-line-numbers-type 'relative)
  :hook ((prog-mode
          conf-mode toml-ts-mode
          yaml-mode yaml-ts-mode)
         . display-line-numbers-mode)
  :bind
  (:map global-map
        ("<insert>" . nil)
        ("<menu>" . nil)
        ("C-z" . nil)
        ("C-M-w" . nil)
        ("C-x C-z" . nil)
        ("C-x C-d" . nil)
        ("C-x C-v" . nil)
        ("C-x C-c" . nil)
        ("C-x C-c C-c" . save-buffers-kill-emacs)
        ("C-x C-r" . restart-emacs) ; override `find-file-read-only'
        ("M-c" . capitalize-dwim)
        ("M-l" . downcase-dwim)
        ("M-u" . upcase-dwim)
        ("M-=" . count-words)
        ("M-:" . pp-eval-expression)
        ;; ("C-'" . duplicate-dwim) ;; NOTE: original bind with undo
        ;; ("C-w" . backward-kill-word)
        ("C-h K" . describe-keymap)
        ))

(use-package cw-simple
  :straight nil
  :commands (cw-simple-override-mode)
  :config
  (cw-simple-override-mode 1)
  :bind
  (:map global-map
        ("<escape>" . cw-simple-keyboard-quit-dwim)
        ("C-." . cw-simple-duplicate-line-or-region)
        ("C-g" . cw-simple-keyboard-quit-dwim)
        ("C-M-SPC" . cw-simple-mark-sexp)
        ("C-x 0" . cw-simple-delete-window-dwim) ;; override `delete-window'
        ("C-w" . cw-simple-kill-region-dwim)
        ("M-w" . cw-simple-kill-ring-save-dwim)
        ("C-s-y" . cw-simple-yank-replace-line-or-region)
        ("C-=" . cw-simple-insert-date)
        ("M-r" . window-layout-transpose) ;; Emacs 31 override `move-to-window-line-top-bottom'
        ("M-s-r" . rotate-windows-back)   ;; Emacs 31
        ("C-x o" . cw-simple-other-window)
        ("C-x k" . cw-simple-kill-buffer-current)))

(use-package savehist
  :straight nil
  :commands (savehist-mode savehist-save)
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 100)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-to-list 'savehist-additional-variables 'kill-ring))

(use-package saveplace
  :straight nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode))

(use-package super-save
  :straight t
  :commands super-save-mode
  :hook (after-init . super-save-mode)
  :config
  (setq super-save-remote-files nil)
  (setq super-save-silent t)
  (setq super-save-delete-trailing-whitespace t)
  (setq super-save-auto-save-when-idle nil)
  (setq super-save-all-buffers nil)
  (add-to-list 'super-save-hook-triggers 'find-file-hook))

(use-package autorevert
  :straight nil
  :commands (auto-revert-mode global-auto-revert-mode)
  :hook
  (after-init . global-auto-revert-mode)
  :config
  (setq revert-without-query (list "."))
  (setq auto-revert-verbose t)
  (setq auto-revert-interval 3)
  (setq auto-revert-use-notify t)
  (setq auto-revert-check-vc-info t)
  (setq auto-revert-remote-files nil)
  (setq auto-revert-avoid-polling nil)
  (setq auto-revert-stop-on-user-input nil)
  (setq global-auto-revert-non-file-buffers t)
  ;; Resolve issue #29
  (setq global-auto-revert-ignore-modes '(Buffer-menu-mode)))

(use-package recentf
  :straight nil
  :commands (recentf-mode recentf-cleanup)
  :hook
  (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$"
         "\\.bz2$" "\\.bz$" "\\.gz$" "\\.gzip$"
         "\\.xz$" "\\.zip$" "\\.7z$" "\\.rar$"

         "^/tmp/"
         "^/var/folders/.+$"
         "^/usr/include/"

         "/ssh:"
         "/sudo:"

         ".cask"
         "url"

         "\\.?cache"
         "/.elfeed/"

         "/TAGS\\'"
         "/G?TAGS$"
         "savehist"
         "bookmarks"
         "saveplaces"
         "undo-tree-hist"

         "\\.revive$"
         "persp-confs"
         "/persp-confs/"
         "\\.?ido\\.last$"

         "autoload\\.el$"
         "-autoloads\\.el$"
         "\\(?:\\.emacs\\.d\\|emacs\\)/\\(?:elpa\\|straight\\|elpaca\\)"

         "COMMIT_EDITMSG\\'"

         "\\.\\(?:pdf\\|docx?\\|xlsx?\\)$"
         "\\.\\(?:gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"))
  :config
  ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
  ;; `recentf-save-list', allowing stale entries to be removed before the list
  ;; is saved by `recentf-save-list', which is automatically added to
  ;; `kill-emacs-hook' by `recentf-mode'.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

(use-package repeat
  :straight nil
  :commands repeat-mode
  :hook (after-init . repeat-mode)
  :config
  (setq repeat-on-final-keystroke t
        repeat-exit-timeout 5
        repeat-exit-key "<escape>"
        repeat-keep-prefix nil
        repeat-check-key t
        repeat-echo-function 'ignore
        set-mark-command-repeat-pop t
        )
  )

(use-package compile
  :straight nil
  :hook (compilation-filter . +compilation-colorize-filter-h)
  :hook (shell-mode . compilation-shell-minor-mode)
  :bind ("C-;" . compile)
  :custom
  (compilation-scroll-output 'first-error) ; Keep scrolling the compilation buffer, `first-error' can be interesting
  (compilation-always-kill t) ; Always kill current compilation process before starting a new one
  (compilation-skip-visited t) ; Skip visited messages on compilation motion commands
  (compilation-window-height 12) ; Keep it readable  :init
  :config
  (defconst +compilation-extra-escape-sequences-regexp
    (concat "\e"
            (regexp-opt-charset '(?\( ?\) ?* ?+ ?- ?. ?/))
            "[\x30-\x7E]")
    "Regexp matching ISO-2022 escape sequences unsupported by `ansi-color'.")

  (defun +compilation--strip-extra-escape-sequences (start end)
    (save-excursion
      (save-match-data
        (let ((end-marker (copy-marker end))
              (start-pos (if (markerp start) (marker-position start) start)))
          (goto-char (max (point-min) (- start-pos 2)))
          (while (re-search-forward +compilation-extra-escape-sequences-regexp end-marker t)
            (replace-match "" t t))))))

  (defun +compilation-colorize-filter-h ()
    "Strip unsupported terminal escape sequences before ANSI colorizing."
    (let ((inhibit-read-only t))
      (+compilation--strip-extra-escape-sequences compilation-filter-start (point))
      (ansi-color-compilation-filter)))

  (add-to-list 'compilation-environment "TERM=xterm-256color")
  ;; Integration of `compile' with `savehist'
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'compile-history)))

(provide 'init-basic)
