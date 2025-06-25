;;; -*- lexical-binding: t -*-

(use-package org
  :straight nil
  :init
  (setq org-directory curtain-org-directory
        org-imenu-depth 7)
  (add-to-list 'safe-local-variable-values '(org-hide-leading-stars . t))
  (add-to-list 'safe-local-variable-values '(org-hide-macro-markers . t))
  ;; (setq org-export-backends '(html textinfo md))
  :custom-face
  (org-document-title ((t (:height 1.75 :weight bold))))
  (org-level-1 ((t (:height 1.2 :weight bold))))
  (org-level-2 ((t (:height 1.15 :weight bold))))
  (org-level-3 ((t (:height 1.1 :weight bold))))
  (org-level-4 ((t (:height 1.05 :weight bold))))
  (org-level-5 ((t (:height 1.0 :weight bold))))
  (org-level-6 ((t (:height 1.0 :weight bold))))
  (org-level-7 ((t (:height 1.0 :weight bold))))
  (org-level-8 ((t (:height 1.0 :weight bold))))
  (org-level-9 ((t (:height 1.0 :weight bold))))
  :bind
  (:map global-map
        ;; ("C-c l" . org-store-link)
        ;; ("C-c o" . org-open-at-point-global)
        ("C-c o p" . org-insert-property-drawer)
        ("C-c o c" . org-capture)
        )
  :config
  (require 'org-tempo)
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
  (setq org-ellipsis "…"
        org-default-notes-file (expand-file-name "all-blogpost.org" curtain-blog-directory)
        org-yank-image-save-method "."
        org-startup-truncated nil
        org-startup-folded 'show2levels
        org-image-actual-width nil
        org-adapt-indentation nil
        org-special-ctrl-a/e nil
        org-special-ctrl-k nil
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-hide-macro-markers t
        org-cycle-separator-lines 0
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
        org-fold-catch-invisible-edits 'show
        org-return-follows-link nil
        org-loop-over-headlines-in-active-region 'start-level
        org-use-sub-superscripts '{}
        org-insert-heading-respect-content t
        org-read-date-prefer-future 'time
        org-fontify-whole-block-delimiter-line t
        org-fontify-quote-and-verse-blocks t
        org-track-ordered-property-with-tag t
        org-highest-priority ?A
        org-lowest-priority ?C
        org-default-properties ?A)

  ;; (add-hook 'org-mode-hook  (lambda ()
  ;;                             (setq prettify-symbols-alist
  ;;                                   '(("lambda" . ?λ)
  ;;                                     (":END:" . ?)
  ;;                                     ("#+TITLE:" . ?)
  ;;                                     ("#+AUTHOR:" . ?)
  ;;                                     ("#+BEGIN_QUOTE" . ?)
  ;;                                     ("#+END_QUOTE" . ?)
  ;;                                     ("#+RESULTS:" . ?)
  ;;                                     ("[ ]" . ?)
  ;;                                     ("[-]" . ?)
  ;;                                     ("[X]" . ?)
  ;;                                     ("[#A]" . ?🅐)
  ;;                                     ("[#B]" . ?🅑)
  ;;                                     ("[#C]" . ?🅒)))
  ;;                             (prettify-symbols-mode)))
  (add-hook 'org-mode-hook (lambda ()
                             (progn
                               (auto-fill-mode)
                               (setq-local electric-pair-inhibit-predicate
                                         `(lambda (c)
                                            (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))
                             ))

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
        org-fontify-done-headline t
        org-fontify-todo-headline t
        org-fontify-whole-heading-line t
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
  (setq org-confirm-babel-evaluate nil
        org-src-window-setup 'current-window
        org-edit-src-persistent-message nil
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0)
  ;; export
  (setq org-export-with-toc t
        org-export-headline-levels 8
        org-export-dispatch-use-expert-ui nil
        org-html-htmlize-output-type nil
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil)

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

(use-package org-modern
  :straight t
  :after org
  :custom-face
  (org-modern-tag ((t (:inherit org-verbatim :weight regular :foreground "black" :background "LightGray" :box "black"))))
  :custom
  (org-modern-star 'replace)
  ;; (org-modern-replace-stars "♔♙♖♗♘♲")
  (org-modern-table-vertical 5)
  (org-modern-table-horizontal 2)
  (org-modern-block-fringe nil)
  (org-modern-hide-stars nil)
  (org-modern-todo-faces
   ;; Tweak colors, and force it to be monospaced, useful when using `mixed-pitch-mode'.
   '(("IDEA" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "goldenrod"))
     ("NEXT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "IndianRed1"))
     ("STRT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "OrangeRed"))
     ("WAIT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "coral"))
     ("KILL" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "DarkGreen"))
     ("PROJ" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "LimeGreen"))
     ("HOLD" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "orange"))
     ("DONE" . (:inherit org-verbatim :weight semi-bold :foreground "black" :background "LightGray"))))
  :hook
  ((org-mode . org-modern-mode)
   (org-mode . org-indent-mode)
   (org-agenda-finalize . org-modern-agenda-mode))
  )

(use-package org-rich-yank
  :disabled
  :after org
  :hook (org-mode . org-rich-yank-enable))

(provide 'init-org)
