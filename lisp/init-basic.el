;;; -*- lexical-binding: t -*-

(setq-default
 ;; no client startup messages
 server-client-instructions nil

 create-lockfiles nil
 make-backup-files nil
 auto-save-default t
 auto-save-include-big-deletions t ; Don't auto-disable auto-save after deleting big chunks.
 auto-save-list-file-prefix (expand-file-name "autosaves/" user-emacs-directory)
 auto-save-file-name-transforms (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                                            ;; Prefix tramp autosaves to prevent conflicts with local ones
                                            (concat auto-save-list-file-prefix "tramp-\\2") t)
                                      (list ".*" auto-save-list-file-prefix t))

 ;; Disable [bidirectional text] scanning for a modest performance
 ;; Will improve long line display performance
 bidi-inhibit-bpa t
 bidi-paragraph-direction 'left-to-right
 bidi-display-reordering 'left-to-right

 ;; smaller threshold to improve long line performance
 long-line-threshold 1000
 large-hscroll-threshold 1000
 syntax-wholeline-max 1000

 ;; Larger process output buffer for LSP module
 read-process-output-max (* 3 1024 1024)

 ;; [Wrapping] words at whitespace, but do not wrap by default
 ;; Wrap words at whitespace, rather than in the middle of a word.
 word-wrap t
 ;; don't do any wrapping by default since it's expensive
 truncate-lines t
 truncate-partial-width-windows nil
 ;; better wrapping for cjk
 word-wrap-by-category t

 ;; Custom file path
 custom-file (expand-file-name "custom.el" user-emacs-directory)

 ;; Always follow link when visiting a [symbolic link]
 find-file-visit-truename t
 vc-follow-symlinks t

 ;; Case insensitive completion
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t

 ;; disable [bell] completely
 ring-bell-function 'ignore

 ;; Disable copy region blink
 copy-region-blink-delay 0
 delete-pair-blink-delay 0

 ;; set [fill column] indicator to 100
 fill-column 100

 help-window-select t
 next-error-recenter '(4)
 find-library-include-other-files nil
 remote-file-name-inhibit-delete-by-moving-to-trash t
 remote-file-name-inhibit-auto-save t
 tramp-connection-timeout (* 60 10)
 duplicate-line-final-position -1
 duplicate-region-final-position -1
 kill-do-not-save-duplicates t
 scroll-error-top-bottom t
 echo-keystrokes-help nil

 ;; [tab]
 ;; Make `tabify' only affect indentation
 tabify-regexp "^\t* [ \t]+"
 ;; Indent with 4 space by default
 indent-tabs-mode nil
 tab-always-indent t
 tab-width 4

 ;; Sentence end
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
 sentence-end-double-space nil

 ;; Use y-or-n to replace yes-or-no
 use-short-answers t
 ;; Inhibit switching out from `y-or-n-p' and `read-char-choice'
 y-or-n-p-use-read-key t
 read-char-choice-use-read-key t

 ;; Don't ping things that look like domain names.
 ffap-machine-p-known 'reject

 ;; Disable the "same file" warning, just redirect to the existing buffer
 find-file-suppress-same-file-warnings t

 ;; POSIX standard [newline]
 require-final-newline t

 ;; Don't prompt for confirmation when creating a new file or buffer
 confirm-nonexistent-file-or-buffer nil

 ;; Show path/name if names are same
 uniquify-buffer-name-style 'forward

 ;; Fix alignment problem
 truncate-string-ellipsis "…"

 ;; Shell command
 shell-command-prompt-show-cwd t

 ;; What-cursor-position
 what-cursor-show-names t

 ;; List only applicable commands
 read-extended-command-predicate #'command-completion-default-include-p
 )

;; unbind some annoying commands
(keymap-global-unset "C-z" 'remove)
(keymap-global-unset "C-x C-z" 'remove)

;; remap some keys
(keymap-global-set "M-:" #'pp-eval-expression)
(keymap-global-set "C-c f" #'recentf)

;; Enable the disabled dired commands
(put 'dired-find-alternate-file 'disabled nil)

;; fullscreen with maximized
(setq initial-frame-alist '((fullscreen . maximized)))

;; Encoding & locale
(set-locale-environment "en_US.UTF-8")
(setq-default default-input-method nil)
(setq system-time-locale "C")

(use-package emacs
  :demand t
  :bind
  (:map global-map
        ("M-c" . capitalize-dwim)
        ("M-l" . downcase-dwim)
        ("M-u" . upcase-dwim)
        ("M-=" . count-words)
        ("C-h K" . describe-keymap)
        ))
 
;;; [recentf] recently visited files
(use-package recentf
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-auto-cleanup nil
        recentf-max-saved-items 100
        recentf-max-menu-items 25
        recentf-save-file-modes nil
        recentf-initialize-file-name-history nil
        recentf-filename-handlers nil
        recentf-show-file-shortcuts-flag nil
        recentf-exclude (list "\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
		                      "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
		                      "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
		                      (lambda (file) (file-in-directory-p file package-user-dir))
		                      (expand-file-name recentf-save-file))
        recentf-keep nil)
  )

(use-package repeat
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

(use-package bookmark
  :commands (bookmark-set bookmark-jump bookmark-bmenu-list)
  :hook (bookmark-bmenu-mode . hl-line-mode)
  :config
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations nil)
  (setq bookmark-fringe-mark nil) ; Emacs 29 to hide bookmark fringe icon
  ;; Write changes to the bookmark file as soon as 1 modification is
  ;; made (addition or deletion).  Otherwise Emacs will only save the
  ;; bookmarks when it closes, which may never happen properly
  ;; (e.g. power failure).
  (setq bookmark-save-flag 1))


(use-package time
  :commands (world-clock)
  :config
  (setq display-time-world-list t)
  (setq zoneinfo-style-world-list ; M-x shell RET timedatectl list-timezones
        '(("America/Los_Angeles" "Los Angeles")
          ("America/Vancouver" "Vancouver")
          ("Canada/Pacific" "Canada/Pacific")
          ("America/Chicago" "Chicago")
          ("Brazil/Acre" "Rio Branco")
          ("America/Toronto" "Toronto")
          ("America/New_York" "New York")
          ("Canada/Atlantic" "Canada/Atlantic")
          ("Brazil/East" "Brasília")
          ("UTC" "UTC")
          ("Europe/Lisbon" "Lisbon")
          ("Europe/Brussels" "Brussels")
          ("Europe/Athens" "Athens")
          ("Asia/Riyadh" "Riyadh")
          ("Asia/Tehran" "Tehran")
          ("Asia/Tbilisi" "Tbilisi")
          ("Asia/Yekaterinburg" "Yekaterinburg")
          ("Asia/Kolkata" "Kolkata")
          ("Asia/Singapore" "Singapore")
          ("Asia/Shanghai" "Shanghai")
          ("Asia/Seoul" "Seoul")
          ("Asia/Tokyo" "Tokyo")
          ("Asia/Vladivostok" "Vladivostok")
          ("Australia/Brisbane" "Brisbane")
          ("Australia/Sydney" "Sydney")
          ("Pacific/Auckland" "Auckland")))

  ;; All of the following variables are for Emacs 28
  (setq world-clock-list t)
  (setq world-clock-time-format "%R %z (%Z)	%A %d %B")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  (setq world-clock-timer-enable t)
  (setq world-clock-timer-second 60))

(use-package proced
  :commands (proced)
  :config
  (setq proced-auto-update-flag 'visible) ; Emacs 30 supports more the `visible' value
  (setq proced-enable-color-flag t) ; Emacs 29
  (setq proced-auto-update-interval 5)
  (setq proced-descend t)
  (setq proced-filter 'user))


;; [so-long] Workaround for long one-line file
(use-package so-long
  :hook ((after-init . global-so-long-mode)
	 ((so-long-mode prog-mode fundamental-mode) . +so-long-settings))
  :config
  ;; improve long line performance
  (defun +so-long-settings ()
    (setq bidi-display-reordering nil))

  ;; Saveplace should not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  )

(setq
 ;; Performant and rapid scrolling
 fast-but-imprecise-scrolling t

 ;; Keep 5 lines when scrolling
 scroll-step 0
 scroll-margin 3
 scroll-up-aggressively 0.01 ; less jumpy
 scroll-down-aggressively 0.01
 scroll-conservatively 101
 ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll' for tall lines.
 auto-window-vscroll nil

 ;; [hscroll]
 auto-hscroll-mode t
 hscroll-step 0
 hscroll-margin 2)

;; (pixel-scroll-precision-mode)

(defvar +scrolling-lines 10)
(defun +scroll-other-window () (interactive) (scroll-other-window +scrolling-lines))
(defun +scroll-other-window-down () (interactive) (scroll-other-window-down +scrolling-lines))
(defun +scroll-window () (interactive) (scroll-up +scrolling-lines))
(defun +scroll-window-down () (interactive) (scroll-down +scrolling-lines))
(bind-keys*
 ("C-M-v" . +scroll-other-window)
 ("M-<down>" . +scroll-other-window)

 ("C-M-S-v" . +scroll-other-window-down)
 ("M-<up>" . +scroll-other-window-down)

 ("C-v" . +scroll-window-down)
 ("M-v" . +scroll-window))


(use-package display-line-numbers
  :hook ((prog-mode
          conf-mode toml-ts-mode
          yaml-mode yaml-ts-mode)
         . display-line-numbers-mode)
  :init
  (setq display-line-numbers-width-start t))

(use-package dired
  :config
  (setq
   ;; Always delete and copy recursively
   dired-recursive-deletes 'top
   dired-recursive-copies 'always
   ;; Move between two dired buffer quickly
   dired-dwim-target t
   ;; Ask whether destination dirs should get created when copying/removing files.
   dired-create-destination-dirs 'ask
   ;; don't prompt to revert, just do it
   dired-auto-revert-buffer #'dired-buffer-stale-p
   ;; symlink
   dired-hide-details-hide-symlink-targets nil
   dired-auto-revert-buffer #'dired-directory-changed-p
   dired-make-directory-clickable t
   dired-free-space nil
   dired-mouse-drag-files t
   )

  ;;(add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)
  (define-key dired-jump-map (kbd "j") nil)

  (when (eq system-type 'darwin)
    (if (executable-find "gls")
        (setq insert-directory-program "gls") ; Use GNU ls as `gls' from `coreutils' if available.
      ;; Suppress the warning: `ls does not support --dired'.
      (setq dired-use-ls-dired nil)))

  (when (or (not (eq system-type 'darwin)) (executable-find "gls"))
    (setq ls-lisp-use-insert-directory-program t ; Using `insert-directory-program'
          ;; Show directory first
          dired-listing-switches "-alh --group-directories-first"))
  )

(use-package wdired
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(use-package diredfl
  :straight t
  :hook (dired-mode . diredfl-mode)
  :config
  (cl-callf append diredfl-compressed-extensions '(".zst" ".rar" ".7z" ".cab" ".arc" ".zoo")))

(use-package compile
  :hook (compilation-filter . ansi-color-compilation-filter) ; Enable ANSI colors in compilation buffer
  :hook (shell-mode . compilation-shell-minor-mode)
  :bind ("C-;" . compile)
  :custom
  (compilation-scroll-output t) ; Keep scrolling the compilation buffer, `first-error' can be interesting
  (compilation-always-kill t) ; Always kill current compilation process before starting a new one
  (compilation-skip-visited t) ; Skip visited messages on compilation motion commands
  (compilation-window-height 12) ; Keep it readable  :init
  :config
  ;; Integration of `compile' with `savehist'
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'compile-history)))

(use-package simpc-mode
  :straight nil
  :demand t
  :init
  (add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode)))


(column-number-mode 1)
(show-paren-mode 1)


(use-package yasnippet
  :straight t
  :hook ((text-mode prog-mode conf-mode) . yas-minor-mode)
  :custom
  (yas-triggers-in-field t))


(provide 'init-basic)
