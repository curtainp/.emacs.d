;;; -*- lexical-binding: t -*-


(use-package org
  ;; :straight nil
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . my/org-prettify-symbols))
  :custom-face
  ;; 设置Org mode标题以及每级标题行的大小
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
  ;; 设置代码块用上下边线包裹
  (org-block-begin-line ((t (:underline t :background unspecified))))
  (org-block-end-line ((t (:overline t :underline nil :background unspecified))))
  :custom
  (org-directory curtain-org-directory)
  (org-imenu-depth 4)
  (org-ellipsis " ↩")
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-fontify-whole-heading-line t)    ;; prettify heading line
  (org-fontify-todo-headline t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-macro-markers t)
  (org-highlight-latex-and-related '(native script entities))
  (org-startup-indented t)
  (org-adapt-indentation t)
  ;; (org-startup-with-inline-images t)
  (org-startup-folded 'overview)
  (org-list-allow-alphabetical t)
  (org-list-demote-modify-bullet '(     ;; sublist bullet config
                                   ("-" . "+")
                                   ("+" . "1.")
                                   ("1." . "a.")
                                   ))
  (org-fold-catch-invisible-edits 'smart)
  (org-insert-heading-respect-content nil)
  (org-image-actual-width nil)
  (org-yank-image-save-method "imgs")
  (org-return-follows-link nil) 
  (org-use-sub-superscripts '{})        ;; use {} 包裹上下标
  (org-clone-delete-id t)
  (org-yank-adjusted-subtrees t)
  (org-todo-keywords '((sequence "TODO(t)" "HOLD(h!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
                       (sequence "REPEAT(r)" "BUG(b)" "|" "FIXED(f!)")))
  (org-todo-keyword-faces '(("TODO"       :foreground "#7c7c75" :weight bold)
                            ("HOLD"       :foreground "#feb24c" :weight bold)
                            ("DONE"       :foreground "#50a14f" :weight bold)
                            ("CANCELLED"  :foreground "#ff6480" :weight bold)
                            ("REPORT"     :foreground "magenta" :weight bold)
                            ("BUG"        :foreground "red"     :weight bold)
                            ("FIXED"      :foreground "green"   :weight bold)))
  (org-todo-state-tags-triggers
   (quote (("CANCELLED" ("CANCELLED" . t))
           ("HOLD" ("HOLD" . t))
           (done ("HOLD"))
           ("TODO" ("CANCELLED") ("HOLD"))
           ("DONE" ("CANCELLED") ("HOLD")))))
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
  (org-tag-alist '((:startgroup)
                   ("crypto"   . ?c)
                   ("linux"    . ?l)
                   ("noexport" . ?n)
                   ("ignore"   . ?i)
                   ("toc"      . ?t)
                   (:endgroup)))
  (org-structure-template-alist
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
               ("q" . "quote")))

  :config
  (defun my/org-prettify-symbols ()
    (setq prettify-symbols-alist
          (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                  '(
                    ;; ("[ ]"              . 9744)         ; ☐
					;; ("[X]"              . 9745)         ; ☑
					;; ("[-]"              . 8863)         ; ⊟
					("#+begin_src"      . 9998)         ; ✎
					("#+end_src"        . 9633)         ; □
					("#+begin_example"  . 129083)       ; 🠻
					("#+end_example"    . 129081)       ; 🠹
					("#+results:"       . 9776)         ; ☰
					("#+attr_latex:"    . "🄛")
					("#+attr_html:"     . "🄗")
					("#+attr_org:"      . "🄞")
					("#+name:"          . "🄝")         ; 127261
					("#+caption:"       . "🄒")         ; 127250
					("#+date:"          . "📅")         ; 128197
					("#+author:"        . "💁")         ; 128100
					("#+setupfile:"     . 128221)       ; 📝
					("#+email:"         . 128231)       ; 📧
					("#+startup:"       . 10034)        ; ✲
					("#+options:"       . 9965)         ; ⛭
					("#+title:"         . "📝")        ; 📝
                    ("#+draft:"         . "🚧")
                    ("#+tags[]:"        . "📌")
                    ("#+categories[]:"  . "🔖")
					("#+subtitle:"      . 11146)        ; ⮊
					("#+downloaded:"    . 8650)         ; ⇊
					("#+language:"      . 128441)       ; 🖹
					("#+begin_quote"    . 187)          ; »
					("#+end_quote"      . 171)          ; «
                    ("#+begin_results"  . 8943)         ; ⋯
                    ("#+end_results"    . 8943)         ; ⋯
                    )))
    (setq prettify-symbols-unprettify-at-point t)
    (prettify-symbols-mode))
  ;; (plist-put org-format-latex-options :scale 1.8)
  (when (executable-find "dvisvgm")
    (setopt org-preview-latex-default-process 'dvisvgm)
    (setq org-preview-latex-image-directory (expand-file-name "~/.cache/org/preview/latex-image/")))
  )

(use-package org-modern
  :straight t
  :after org
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-table-vertical 2)
  (org-modern-star 'replace)
  (org-modern-replace-stars "◉○✸✳◈◇✿❀✜")
  (setq-default line-spacing 0.1)       ;; 0.1 indicate 10% for extra line spacing
  (org-modern-table-horizontal 0)
  (org-modern-label-border 1)
  (org-modern-block-fringe t)
  (org-modern-block-name nil)           ;; use `prettify-symbols-mode' instead
  (org-modern-keyword nil)
  ;; 复选框美化
  (setq org-modern-checkbox
        '((?X . #("▢✓" 0 2 (composition ((2)))))
          (?- . #("▢–" 0 2 (composition ((2)))))
          (?\s . #("▢" 0 1 (composition ((1)))))))
  ;; 列表符号美化
  (setq org-modern-list
        '((?- . "•")
          (?+ . "◦")
          (?* . "▹"))))

(use-package org-appear
  :straight t
  :after org
  :hook (org-mode . org-appear-mode))

;; preview and edit latex in org elegantly
(use-package org-fragtog
  :straight t
  :hook (org-mode . org-fragtog-mode))

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

(use-package ox-latex
  :straight nil
  :custom
  (org-latex-src-block-backend 'engraved))

(use-package org-src
  :straight nil
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :bind (("s-l" . show-line-number-in-src-block)
         :map org-src-mode-map
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

  ;; ==============================================
  ;; 通过overlay在代码块里显示行号，s-l显示，任意键关闭
  ;; ==============================================
  (defvar number-line-overlays '()
    "List of overlays for line numbers.")

  (defun show-line-number-in-src-block ()
    (interactive)
    (save-excursion
      (let* ((src-block (org-element-context))
             (nlines (- (length
                         (s-split
                          "\n"
                          (org-element-property :value src-block)))
                        1)))
        (goto-char (org-element-property :begin src-block))
        (re-search-forward (regexp-quote (org-element-property :value src-block)))
        (goto-char (match-beginning 0))

        (cl-loop for i from 1 to nlines
                 do
                 (beginning-of-line)
                 (let (ov)
                   (setq ov (make-overlay (point) (point)))
                   (overlay-put ov 'before-string (format "%3s | " (number-to-string i)))
                   (add-to-list 'number-line-overlays ov))
                 (next-line))))

    ;; now read a char to clear them
    (read-key "Press a key to clear numbers.")
    (mapc 'delete-overlay number-line-overlays)
    (setq number-line-overlays '()))

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
                              (calc            . t)
                              (emacs-lisp      . t)
                              (eshell          . t)
                              (shell           . t)
                              (sql             . t)
                              (css             . t)
                              ))
  )

(provide 'init-org)
