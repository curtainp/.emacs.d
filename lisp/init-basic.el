;;; -*- lexical-binding: t -*-

(require 'init-funcs)

(setq user-full-name curtain-full-name
      user-mail-address curtain-email-address)
;; refer https://emacs.stackexchange.com/questions/82010/why-is-emacs-recompiling-some-packages-on-every-startup
(use-package comp-run
  :straight nil
  :config
  (push "tramp-loaddefs.el.gz" native-comp-jit-compilation-deny-list)
  (push "cl-loaddefs.el.gz" native-comp-jit-compilation-deny-list))

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

(setq-default
 create-lockfiles nil
 make-backup-files nil
 auto-save-default t
 scroll-preserve-screen-position 'always
 auto-save-include-big-deletions t ; Don't auto-disable auto-save after deleting big chunks.
 auto-save-list-file-prefix (expand-file-name "autosaves/" user-emacs-directory)
 auto-save-file-name-transforms (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                                            ;; Prefix tramp autosaves to prevent conflicts with local ones
                                            (concat auto-save-list-file-prefix "tramp-\\2") t)
                                      (list ".*" auto-save-list-file-prefix t))
 ;; smaller threshold to improve long line performance
 long-line-threshold 1000
 large-hscroll-threshold 1000
 syntax-wholeline-max 1000

 column-number-mode t
 ;; Larger process output buffer for LSP module
 read-process-output-max (* 4 1024 1024)
 visible-bell t
 ;; Wrap words at whitespace, rather than in the middle of a word.
 word-wrap t
 ;; don't do any wrapping by default since it's expensive
 truncate-lines t
 truncate-partial-width-windows nil
 ;; better wrapping for cjk
 word-wrap-by-category t

 ;; Always follow link when visiting a [symbolic link]
 find-file-visit-truename t
 vc-follow-symlinks t

 ;; Case insensitive completion
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t

 ;; disable [bell] completely
 ring-bell-function 'ignore

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

 ;; indent offset for language
 c-basic-offset 4

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

 ;; disable `tramp-mode'
 tramp-mode nil

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

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers nil)

;; make underscore as part of the word
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

;; Enable the disabled dired commands
(put 'dired-find-alternate-file 'disabled nil)

;; Encoding & locale
(set-locale-environment "en_US.UTF-8")
(setq-default default-input-method nil)
(setq system-time-locale "C")

(use-package emacs
  :straight nil
  :bind
  (:map global-map
        ("<insert>" . nil)
        ("<menu>" . nil)
        ("C-z" . nil)
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

(use-package curt-simple
  :straight nil
  :config
  (curt-simple-override-mode 1)
  :bind
  (:map global-map
        ("ESC ESC" . curt-simple-keyboard-quit-dwim)
        ("C-g" . curt-simple-keyboard-quit-dwim)
        ("C-=" . curt-simple-insert-date)
        ("C-x o" . curt-simple-other-window)
        ("C-x k" . curt-simple-kill-buffer-current)))

;;; [recentf] recently visited files
(use-package recentf
  :straight nil
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
  :straight nil
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
  :straight nil
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

(use-package ultra-scroll
  :straight (:host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(use-package display-line-numbers
  :straight nil
  :custom
  (display-line-numbers-type 'relative)
  :hook ((prog-mode
          conf-mode toml-ts-mode
          yaml-mode yaml-ts-mode)
         . display-line-numbers-mode)
  :init
  (setq display-line-numbers-width-start t))

(use-package compile
  :straight nil
  :hook (compilation-filter . ansi-color-compilation-filter) ; Enable ANSI colors in compilation buffer
  :hook (shell-mode . compilation-shell-minor-mode)
  :bind ("C-;" . compile)
  :custom
  (compilation-scroll-output t) ; Keep scrolling the compilation buffer, `first-error' can be interesting
  (compilation-always-kill t) ; Always kill current compilation process before starting a new one
  (compilation-skip-visited t) ; Skip visited messages on compilation motion commands
  (compilation-window-height 12) ; Keep it readable  :init
  :config
  (add-to-list 'compilation-environment "TERM=xterm-256color")
  ;; Integration of `compile' with `savehist'
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'compile-history)))

(provide 'init-basic)
