;;; -*- lexical-binding: t -*-

(with-eval-after-load 'calendar
  (setq calendar-mark-diary-entries-flag nil)
  (setq calendar-mark-holidays-flag t)
  (setq calendar-mode-line-format nil)
  (setq calendar-time-display-form
        '(24-hours ":" minutes
                   (when time-zone (format "(%s)" time-zone))))
  (setq calendar-week-start-day 1)
  (setq calendar-date-style 'iso)
  (setq calendar-time-zone-style 'numeric)
  )

(use-package org
  :straight t
  :commands (org-mode org-version)
  :hook ((org-mode . my/org-prettify-symbols))
  :hook ((org-agenda-after-show org-follow-link) . (pulsar-recenter-center pulsar-reveal-entry))
  :bind (:map global-map
              ("C-c l" . org-store-link)
              ("C-c o" . org-open-at-point-global)
         :map org-mode-map
              ("C-c C-q" . my/org-set-tags-command)
              ("C-'" . nil)
              ("C-," . nil)
              ("M-;" . nil)
              ("C-c ;" . nil)
              ("C-c C-x C-c" . nil)     ; `org-column'
              ("M-." . org-edit-special)
              ("C-c C-v" . yank-media)
              :map org-src-mode-map
              ("M-," . org-edit-src-exit)
              )
  :custom
  (org-directory cw-emacs-notes-directory)
  (org-imenu-depth 7)
  (org-M-RET-may-split-line '((default . nil))) ; move the end of line before make a new line
  (org-ellipsis " ↩")
  (org-pretty-entities t)
  (org-hide-emphasis-markers nil)
  (org-hide-macro-markers nil)
  (org-hide-leading-stars nil)
  (org-fontify-whole-heading-line nil)    ;; prettify heading line
  (org-fontify-todo-headline nil)
  (org-fontify-done-headline nil)
  (org-fontify-quote-and-verse-blocks t)
  (org-highlight-latex-and-related '(native script entities))
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-startup-with-inline-images t)
  (org-footnote-auto-adjust t)
  (org-image-actual-width '(500))       ;; first try get from ATTR html
  (org-startup-folded 'fold)
  (org-list-allow-alphabetical t)
  (org-fold-catch-invisible-edits 'smart)
  (org-insert-heading-respect-content t)
  (org-yank-image-save-method "imgs")
  (org-return-follows-link t)
  (org-use-sub-superscripts '{})        ;; use {} 包裹上下标
  (org-clone-delete-id t)
  (org-yank-adjusted-subtrees t)
  (org-todo-keywords '((sequence
                        "TODO(t)"
                        "WAITING(w)"    ;; use @/! to log note and timestamp
                        "|"
                        "DONE(d)"
                        "OBSOLETE(o)"
                        "CANCELLED(c)")))
  (org-use-fast-todo-selection 'expert)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-priority-faces '((?A :foreground "red")
                        (?B :foreground "orange")
                        (?C :foreground "yellow")))
  (org-closed-keep-when-no-todo t)
  (org-log-done 'time)
  (org-log-repeat 'time)
  (org-log-redeadline 'note)
  (org-log-reschedule 'note)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  (org-refile-use-cache t)
  (org-refile-targets '((org-agenda-files . (:maxlevel 8))))
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-auto-align-tags t)
  (org-use-tag-inheritance nil)
  (org-use-fast-tag-selection t)
  (org-fast-tag-selection-single-key t)
  (org-track-ordered-property-with-tag t)
  (org-tag-persistent-alist '(("emacs"    . ?m)
                              ("security" . ?s)
                              ("pwn"      . ?p)
                              ("note"     . ?n)))
  (org-tag-alist nil)
  (org-auto-align-tags nil)
  (org-tags-column 0)

  :config
  (with-eval-after-load 'yank-media
    (add-to-list 'yank-media-preferred-types 'image/avif))
  (set-face-attribute 'org-code nil :background
                      (face-attribute 'org-block :background))
  (defun my/org-read-file-name-relative (&optional prompt directory)
    "Read a file name and return a path relative to the current buffer."
    (let* ((base-dir (file-name-directory
                      (or (buffer-file-name) default-directory)))
           (file (read-file-name (or prompt "File: ")
                                 (or directory base-dir)
                                 nil t)))
      (file-relative-name file base-dir)))

  (defun my/org--format-filetags (tags)
    "Return a #+FILETAGS string for TAGS."
    (if (null tags)
        ""
      (concat ":" (mapconcat #'identity tags ":") ":")))

  (defun my/org--set-filetags-line (tagstr)
    "Insert or update #+FILETAGS line with TAGSTR."
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (if (re-search-forward "^#\\+FILETAGS:.*$" nil t)
            (replace-match (concat "#+FILETAGS: " tagstr) t t)
          (let ((insert-pos (point-min)))
            (goto-char (point-min))
            (while (and (not (eobp))
                        (looking-at "^#\\+\\w+:"))
              (setq insert-pos (line-end-position))
              (forward-line 1))
            (goto-char insert-pos)
            (when (not (bolp)) (insert "\n"))
            (insert "#+FILETAGS: " tagstr "\n"))))))

  (defun my/org-set-filetags ()
    "Set #+FILETAGS for the current buffer."
    (interactive)
    (org-set-regexps-and-options)
    (let* ((current (mapcar #'substring-no-properties org-file-tags))
           (table (delete-dups
                   (append (mapcar #'car (org-get-buffer-tags))
                           (mapcar #'car org-tag-persistent-alist))))
           (selected (completing-read-multiple
                      "File tags (comma/space separated): "
                      table nil nil
                      (mapconcat #'identity current ",")))
           (tagstr (my/org--format-filetags selected)))
      (my/org--set-filetags-line tagstr)
      (org-set-regexps-and-options)
      (message "File tags set: %s" tagstr)))

  (defun my/org-set-tags-command (&optional arg)
    "Set tags for current heading or file.
If before first heading, set #+FILETAGS.  Otherwise delegate to
`org-set-tags-command'."
    (interactive "P")
    (if (org-before-first-heading-p)
        (my/org-set-filetags)
      (org-set-tags-command arg)))

  (defun my/org-prettify-symbols ()
    (setq prettify-symbols-alist
          (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                  '(
                    ("#+BEGIN_SRC" . "✎")
                    ("#+END_SRC" . "□")
                    ("#+begin_src" . "✎")
                    ("#+end_src" . "□")
                    ("#+RESULTS:" . "⟾")
                    ("#+begin_quote" . "»")
                    ("#+end_quote" . "□")
                    ("#+begin_verse" . "ζ")
                    ("#+end_verse" . "□")
                    ("#+begin_example" . "⟝")
                    ("#+end_example" . "□")
                    ("#+begin_export" . "🙵")
                    ("#+end_export" . "□")
                    ("#+BEGIN_QUOTE" . "»")
                    ("#+END_QUOTE" . "□")
                    ("#+BEGIN_VERSE" . "ζ")
                    ("#+END_VERSE" . "□")
                    ("#+BEGIN_EXAMPLE" . "⟝")
                    ("#+END_EXAMPLE" . "□")
                    ("#+BEGIN_EXPORT" . "🙵")
                    ("#+END_EXPORT" . "□")
                    ("#+END:" . "□")
                    ("#+BEGIN:" . "✎")
                    ("#+CAPTION:" . "✑")
					("#+attr_latex:"    . "🄛")
					("#+attr_html:"     . "🄗")
					("#+attr_org:"      . "🄞")
					;; ("#+name:"          . "🄝")         ; 127261
					;; ("#+caption:"       . "🄒")         ; 127250
					;; ("#+date:"          . "📅")         ; 128197
					;; ("#+author:"        . "💁")         ; 128100
					;; ("#+setupfile:"     . 128221)       ; 📝
					;; ("#+email:"         . 128231)       ; 📧
					;; ("#+startup:"       . 10034)        ; ✲
					;; ("#+options:"       . 9965)         ; ⛭
					;; ("#+title:"         . "📝")        ; 📝
                    ;; ("#+draft:"         . "🚧")
                    ;; ("#+tags[]:"        . "📌")
                    ;; ("#+categories[]:"  . "🔖")
                    )))
    (setq prettify-symbols-unprettify-at-point t)
    (prettify-symbols-mode))
  )


(use-package org-modern
  :disabled
  :straight t
  :commands org-modern-mode
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-checkbox
   '((?X . "☑")
     (?- . "◪")
     (?\s . "☐")))
  (org-modern-table-vertical 2)
  (org-modern-star 'replace)
  (org-modern-replace-stars "◉○✸✳◈◇✿❀✜")
  (setq-default line-spacing 0.1)       ;; 0.1 indicate 10% for extra line spacing
  (org-modern-table nil)
  ;; (org-modern-table-horizontal 0)
  (org-modern-label-border 1)
  (org-modern-block-fringe t)
  (org-modern-block-name nil)           ;; use `prettify-symbols-mode' instead
  (org-modern-keyword nil)
  ;; 列表符号美化
  (org-modern-list
        '((?- . "•")
          (?+ . "◌")
          (?* . "✦")))
  )

(use-package mixed-pitch
  :straight t
  :commands mixed-pitch-mode
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-variable-pitch-cursor 'box
        mixed-pitch-set-height t)
  (dolist (face '(org-date org-tag curfu-default font-lock-comment-face))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

;; preview and edit latex in org elegantly
(use-package org-fragtog
  :disabled
  :straight t
  :hook (org-mode . org-fragtog-mode)
  :config
  (plist-put org-format-latex-options :scale 2.4)
  (when (executable-find "dvisvgm")
    (setq org-preview-latex-default-process 'dvisvgm)
    (setq org-preview-latex-image-directory (expand-file-name "~/.cache/org/preview/latex-image/")))
  )

(use-package org-src
  :straight nil
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :bind (:map org-src-mode-map
         ("C-c C-c" . org-edit-src-exit))
  :init
  ;; 设置代码块的默认头参数
  (setq org-babel-default-header-args
        '(
          (:eval    . "never-export")     ; 导出时不执行代码块
          (:session . "none")
          (:results . "replace")          ; 执行结果替换
          (:exports . "both")             ; 导出代码和结果
          (:cache   . "no")
          (:noweb   . "no")
          (:hlines  . "no")
          (:wrap    . "results")          ; 结果通过#+begin_results包裹
          (:tangle  . "no")               ; 不写入文件
          ))
  :config
  ;; ==================================
  ;; 如果出现代码运行结果为乱码，可以参考：
  ;; https://github.com/nnicandro/emacs-jupyter/issues/366
  ;; ==================================
  (defun display-ansi-colors ()
    (ansi-color-apply-on-region (point-min) (point-max)))
  (add-hook 'org-babel-after-execute-hook #'display-ansi-colors)

  ;; =================================================
  ;; 执行结果后，如果结果所在的文件夹不存在将自动创建
  ;; =================================================
  (defun check-directory-exists-before-src-execution (orig-fun
                                                      &optional arg
                                                      info
                                                      params)
    (when (and (assq ':file (cadr (cdr (org-babel-get-src-block-info))))
               (member (car (org-babel-get-src-block-info)) '("mermaid" "ditaa" "dot" "lilypond" "plantuml" "gnuplot" "d2")))
      (let ((foldername (file-name-directory (alist-get :file (nth 2 (org-babel-get-src-block-info))))))
        (if (not (file-exists-p foldername))
            (mkdir foldername)))))
  (advice-add 'org-babel-execute-src-block :before #'check-directory-exists-before-src-execution)

  ;; =================================================
  ;; 自动给结果的图片加上相关属性
  ;; =================================================
  (setq original-image-width-before-del "400") ; 设置图片的默认宽度为400
  (setq original-caption-before-del "")        ; 设置默认的图示文本为空

  (defun insert-attr-decls ()
    "insert string before babel execution results"
    (insert (concat "\n#+CAPTION:"
                    original-caption-before-del
                    "\n#+ATTR_ORG: :width "
                    original-image-width-before-del
                    "\n#+ATTR_LATEX: :width "
                    (if (>= (/ (string-to-number original-image-width-before-del) 800.0) 1)
                        "1.0"
                      (number-to-string (/ (string-to-number original-image-width-before-del) 800.0)))
                    "\\linewidth :float nil"
                    "\n#+ATTR_HTML: :width "
                    original-image-width-before-del
                    )))

  (defun insert-attr-decls-at (s)
    "insert string right after specific string"
    (let ((case-fold-search t))
      (if (search-forward s nil t)
          (progn
            ;; (search-backward s nil t)
            (insert-attr-decls)))))

  (defun insert-attr-decls-at-results (orig-fun
                                       &optional arg
                                       info
                                       param)
    "insert extra image attributes after babel execution"
    (interactive)
    (progn
      (when (member (car (org-babel-get-src-block-info)) '("mermaid" "ditaa" "dot" "lilypond" "plantuml" "gnuplot" "d2"))
        (setq original-image-width-before-del (number-to-string (if-let* ((babel-width (alist-get :width (nth 2 (org-babel-get-src-block-info))))) babel-width (string-to-number original-image-width-before-del))))
        (save-excursion
          ;; `#+begin_results' for :wrap results, `#+RESULTS:' for non :wrap results
          (insert-attr-decls-at "#+begin_results")))
      (org-redisplay-inline-images)))
  (advice-add 'org-babel-execute-src-block :after #'insert-attr-decls-at-results)

  ;; 再次执行时需要将旧的图片相关参数行删除，并从中头参数中获得宽度参数，参考
  ;; https://emacs.stackexchange.com/questions/57710/how-to-set-image-size-in-result-of-src-block-in-org-mode
  (defun get-attributes-from-src-block-result (&rest args)
    "get information via last babel execution"
    (let ((location (org-babel-where-is-src-block-result))
          ;; 主要获取的是图示文字和宽度信息，下面这个正则就是为了捕获这两个信息
          (attr-regexp "[:blank:]*#\\+\\(ATTR_ORG: :width \\([0-9]\\{3\\}\\)\\|CAPTION:\\(.*\\)\\)"))
      (setq original-caption-before-del "") ; 重置为空
      (when location
        (save-excursion
          (goto-char location)
          (when (looking-at (concat org-babel-result-regexp ".*$"))
            (next-line 2)               ; 因为有个begin_result的抽屉，所以往下2行
            ;; 通过正则表达式来捕获需要的信息
            (while (looking-at attr-regexp)
              (when (match-string 2)
                (setq original-image-width-before-del (match-string 2)))
              (when (match-string 3)
                (setq original-caption-before-del (match-string 3)))
              (next-line)               ; 因为设置了:wrap，所以这里不需要删除这一行
              )
            )))))
  (advice-add 'org-babel-execute-src-block :before #'get-attributes-from-src-block-result)

  :custom
  ;; 代码块语法高亮
  (org-src-fontify-natively t)
  ;; 使用编程语言的TAB绑定设置
  (org-src-tab-acts-natively t)
  ;; 保留代码块前面的空格
  (org-src-preserve-indentation t)
  ;; 代码块编辑窗口的打开方式：当前窗口+代码块编辑窗口
  (org-src-window-setup 'reorganize-frame)
  ;; 执行前是否需要确认
  (org-confirm-babel-evaluate nil)
  ;; 代码块默认前置多少空格
  (org-edit-src-content-indentation 2)
  ;; 代码块的语言模式设置，设置之后才能正确语法高亮
  (org-src-lang-modes '(("C"            . c)
                        ("C++"          . c++)
                        ("bash"         . sh)
                        ("cpp"          . c++)
                        ("elisp"        . emacs-lisp)
                        ("python"       . python)
                        ("shell"        . sh)
                        ("mysql"        . sql)
                        ))
  ;; 在这个阶段，只需要加载默认支持的语言
  (org-babel-load-languages '((python          . t)
                              (awk             . t)
                              (C               . t)
                              (latex           . t)
                              (emacs-lisp      . t)
                              (eshell          . t)
                              (shell           . t)
                              (js              . t)
                              (sql             . t)
                              (css             . t)
                              ))
  )

(use-package ox
  :straight nil
  :config
  (setq org-html-html5-fancy t
        org-html-doctype "html5"))

(use-package valign
  :disabled
  :straight (:host github :repo "casouri/valign")
  :commands valign-mode
  :hook (org-mode . valign-mode))

(provide 'init-org)
