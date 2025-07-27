;;; -*- lexical-binding: t -*-

(use-package org-modern-indent
  :straight (:host github :repo "jdtsmith/org-modern-indent")
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-modern
  :straight t
  :custom
  ;; (org-modern-star 'replace)
  ;; (org-modern-replace-stars "♔♙♖♗♘♲")
  ;; (org-modern-table-vertical 5)
  ;; (org-modern-table-horizontal 2)
  (org-modern-block-fringe nil)
  (org-modern-hide-stars nil)
  (org-modern-list 
   '((?- . "•")
     (?* . "•")
     (?+ . "•")))
  :hook
  (
   (org-mode . org-indent-mode)
   (org-agenda-finalize . org-modern-agenda-mode))
  :init
  (global-org-modern-mode))

(use-package org-appear
  :straight t
  :after org
  :hook (org-mode . org-appear-mode))

(defun my/set-org-font ()
  (interactive)
  ;; org 字体美化
  (require 'org-faces)
  ;; 标题字体大小优化
  (set-face-attribute 'org-document-title nil :weight 'bold :height 1.2)
  (dolist (face '((org-level-1 . 1.15)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.0)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :weight 'medium :height (cdr face)))

  (set-face-attribute 'org-block nil :foreground 'unspecified' :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-begin-line nil :foreground 'unspecified' :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-block-end-line nil :foreground 'unspecified' :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-property-value nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil  :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-drawer nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-document-info-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (setq org-fontify-quote-and-verse-blocks t) ; 启用 org-qoute 变量为 quote 设置不同的字体
  (set-face-attribute 'org-quote nil :inherit 'fixed-pitch)
  (require 'org-indent) ;; 开启 org-indent 并设设置缩进字体
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch)))

(use-package org
  :straight nil
  :init
  (setq org-directory curtain-org-directory
        org-imenu-depth 7)
  :bind
  (:map global-map
        ;; ("C-c l" . org-store-link)
        ;; ("C-c o" . org-open-at-point-global)
        ("C-c o p" . org-insert-property-drawer)
        ("C-c o c" . org-capture)
        )
  :config
  (require 'org-tempo)
  (my/set-org-font)
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
  (setq org-ellipsis "…"
        org-default-notes-file (expand-file-name "all-blogpost.org" curtain-blog-directory)
        org-yank-image-save-method "."
        org-startup-truncated nil
        org-startup-folded 'show2levels
        org-image-actual-width nil
        ;; org-adapt-indentation nil
        ;; org-special-ctrl-a/e nil
        ;; org-special-ctrl-k nil
        ;; org-hide-emphasis-markers t
        ;; org-pretty-entities t
        ;; org-hide-macro-markers t
        ;; org-cycle-separator-lines 0
        org-structure-template-alist
        '(("s" . "src")
          ("e" . "src emacs-lisp")
          ("p" . "src python :results output")
          ("E" . "src emacs-lisp :results value code :lexical t")
          ("t" . "src emacs-lisp :tangle FILENAME")
          ("T" . "src emacs-lisp :tangle FILENAME :mkdirp yes")
          ("x" . "example")
          ("d" . "details")
          ("u" . "summary")
          ("m" . "mark")
          ("r" . "raw")
          ("X" . "export")
          ("q" . "quote"))
        ;; org-fold-catch-invisible-edits 'show
        ;; org-return-follows-link nil
        ;; org-loop-over-headlines-in-active-region 'start-level
        ;; org-use-sub-superscripts '{}
        ;; org-insert-heading-respect-content t
        ;; org-read-date-prefer-future 'time
        ;; org-fontify-whole-block-delimiter-line t
        ;; org-fontify-quote-and-verse-blocks t
        ;; org-track-ordered-property-with-tag t
        org-highest-priority ?A
        org-lowest-priority ?C
        org-default-properties ?A)

  ;; refile and todo
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 2))
          (nil . (:maxlevel . 2)))
        org-refile-use-outline-path t
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-use-cache t
        org-reverse-note-order nil
        org-todo-keywords
        '((sequence "TODO(t)" "|" "CANCEL(c@)" "DONE(d!)")
          (sequence "PROJECT(p)" "|" "NEXT(n)"))
        org-todo-keyword-faces
        '(("NEXT" :inherit warning))
        org-use-fast-todo-selection 'export
        ;; org-fontify-done-headline t
        ;; org-fontify-todo-headline t
        ;; org-fontify-whole-heading-line t
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t)
  ;; agenda
  (setq org-agenda-tags-column 0
        org-agenda-block-separator ?-
        org-agenda-time-grid '((daily today require-timed)
                               (800 1000 1200 1400 1600 1800 2000)
                               " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string "◀── now ─────────────────────────────────────────────────")
        

  ;; tags
  (setq org-tag-alist nil
        org-auto-align-tags nil
        org-tags-column 0)
  ;; log
  (setq org-log-done 'time
        org-log-into-drawer t
        org-log-note-clock-out nil
        org-log-redeadline 'time
        org-log-reschedule 'time)
  ;; links
  (setq org-link-context-for-files t
        org-link-keep-stored-after-insertion nil
        org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  ;; code blocks
  ;; (setq org-confirm-babel-evaluate nil
  ;;       org-src-window-setup 'current-window
  ;;       org-edit-src-persistent-message nil
  ;;       org-src-fontify-natively t
  ;;       org-src-preserve-indentation t
  ;;       org-src-tab-acts-natively t
  ;;       org-edit-src-content-indentation 0)
  ;; export
  ;; (setq org-export-with-toc t
  ;;       org-export-headline-levels 8
  ;;       org-export-dispatch-use-expert-ui nil
  ;;       org-html-htmlize-output-type nil
  ;;       org-html-head-include-default-style nil
  ;;       org-html-head-include-scripts nil)

  ;; org babel languages
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((perl . t)
                                 (shell . t)
                                 (js . t)
                                 (python . t) ;; refer https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-python.html
                                 (emacs-lisp . t)))

  (with-eval-after-load 'org-capture
    (defun org-hugo-subtree-post-capture-template ()
      (let* ((title (read-from-minibuffer "Post Title: "))
             (fname (org-hugo-slug title)))
        (mapconcat #'identity
                   `(
                     ,(concat "* TODO " title)
                     ":PROPERTIES:"
                     ,(concat ":EXPORT_HUGO_BUNDLE: " fname)
                     ":EXPORT_FILE_NAME: index"
                     ":END:"
                     "\n\n")
                   "\n")))
    (add-to-list 'org-capture-templates
                 '("b"
                   "Hugo Post"
                   plain
                   (file "") ;; use `org-default-notes-file' instead
                   (function org-hugo-subtree-post-capture-template))))
  )


(use-package org-rich-yank
  :disabled
  :after org
  :hook (org-mode . org-rich-yank-enable))

(use-package org-present
  :straight t
  :config
  (defun my/org-present-prepare-slide (buffer-name heading)
    (org-overview)  ; 仅显示顶层标题Show only top-level headlines
    (org-show-entry); 展开当前标题Unfold the current entry
    (org-show-children))   ; 显示当前子标题

  (defun my/org-present-start () ; 开始幻灯片的设置
    (turn-off-evil-mode)
    (setq visual-fill-column-width 110
          visual-fill-column-center-text t) ; 调整显示界面
    ;; 调整字体大小
    (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                       (header-line (:height 4.0) variable-pitch)
                                       (org-document-title (:height 1.75) org-document-title)
                                       (org-code (:height 1.55) org-code)
                                       (org-verbatim (:height 1.55) org-verbatim)
                                       (org-block (:height 1.25) org-block)
                                       (org-block-begin-line (:height 0.7) org-block)))
    (setq header-line-format " ") ; 在标题前加入空行
    (display-line-numbers-mode 0)
    (org-display-inline-images) ; 显示图片
    (read-only-mode 1)) ; 只读模式

  (defun my/org-present-end () ; 重置上述设置
    (setq-local face-remapping-alist 
                '((default variable-pitch default)))      
    (setq header-line-format nil) 
    (org-remove-inline-images)
    (org-present-small)
    (read-only-mode 0)
    (display-line-numbers-mode 1)
    (turn-on-evil-mode))


  (add-hook 'org-present-mode-hook 'my/org-present-start)
  (add-hook 'org-present-mode-quit-hook 'my/org-present-end)
  (add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide))

(provide 'init-org)
