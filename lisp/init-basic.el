;;; init-basic.el  -*- lexical-binding: t  -*-

;; no scroll bar in minibuffer
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-window-scroll-bars
             (minibuffer-window frame) 0 nil 0 nil t)))
(setq x-underline-at-descent-line t)
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(set-language-environment 'chinese-gbk)
(prefer-coding-system 'utf-8-auto)
;; load-path
(use-package exec-path-from-shell
    :straight t
    :init
    (setq exec-path-from-shell-variables '("PATH")
            exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize))

(fset 'yes-or-no-p 'y-or-n-p)
(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 4
              indent-tabs-mode nil)     ; Permanently indent with spaces, never with TABs
(setq visible-bell nil
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save
      inhibit-startup-screen t

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

(use-package smartparens
  :straight t
  :init
  (smartparens-global-mode t)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'rust-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  :config
    (sp-with-modes
        '(c++-mode objc-mode c-mode)
      (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(use-package hungry-delete
  :straight t
  :init
  (global-hungry-delete-mode))

(global-auto-revert-mode t)

(provide 'init-basic)
