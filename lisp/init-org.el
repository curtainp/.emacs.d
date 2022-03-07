;;; init-org.el -*- lexical-binding: t -*-

(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)

(use-package org-super-agenda
  :init
  (require 'org-agenda)
  (define-key org-agenda-keymap "j" #'org-agenda-next-line)
  (define-key org-agenda-mode-map "j" #'org-agenda-next-line)
  ;(define-key org-super-agenda-header-map "j" #'org-agenda-next-line)
  (define-key org-agenda-keymap "k" #'org-agenda-previous-line)
  (define-key org-agenda-mode-map "k" #'org-agenda-previous-line)
  ;(define-key org-super-agenda-header-map "k" #'org-agenda-previous-line)


  (setq org-super-agenda-groups
        '((:name "Important"
           :priority "A")
          (:name "Quick Picks"
           :effort< "0:30")
          (:name "Next Items"
           :tag ("NEXT" "outbox"))
          (:priority<= "B"
           :scheduled future)))
  :config
  (org-super-agenda-mode))

(with-eval-after-load 'org-agenda
  ;; keybindings
 (evil-make-overriding-map org-agenda-mode-map 'normal)
)

(with-eval-after-load 'org
  (progn
    (setq org-directory "~/workspace/docs/org-notes/")
    (defvar org-agenda-dir ""
      "gtd org file location.")
    (setq org-agenda-dir "~/workspace/docs/org-notes/")
    (electric-pair-mode t)
    (add-hook
     'org-mode-hook
     (lambda ()
       (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                      (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
    (require 'org-tempo)
    ;; Allow multiple line Org emphasis markup.
    ;; http://emacs.stackexchange.com/a/13828/115
    (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
    ;; Below is needed to apply the modified `org-emphasis-regexp-components'
    ;; settings from above.
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

    (setq org-agenda-log-mode-items '(clock closed state))
    (setq org-complete-tags-always-offer-all-agenda-tags t)
    (require 'org-compat)
    (require 'org)
    ;; (add-to-list 'org-modules "org-habit")
    (add-to-list 'org-modules 'org-habit)
    (require 'org-habit)

    ;; 调整orghabit 的显示长度
    (setq org-habit-graph-column 60)

    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-targets
          '((nil :maxlevel . 4)
            (org-agenda-files :maxlevel . 4)))
    ;; config stuck project
    (setq org-stuck-projects
          '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

    (setq org-agenda-inhibit-startup t) ;; ~50x speedup
    (setq org-agenda-span 'day)
    (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
    (setq org-agenda-window-setup 'current-window)
    (setq org-log-done t)
    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                  (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))
    (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
    (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
    (setq org-agenda-file-work (expand-file-name "work.org" org-agenda-dir))
    (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
    (setq org-agenda-file-blogposts (expand-file-name "all-posts.org" org-agenda-dir))
    (setq org-agenda-files (list org-agenda-dir))
    (define-key org-agenda-mode-map (kbd "C-p") 'org-agenda-previous-item)
    (define-key org-mode-map (kbd "s-p") 'org-priority)
    (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
             "* TODO [#B] %?\n  %i\n %U"
             :empty-lines 1)
            ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
             "* %?\n  %i\n %U"
             :empty-lines 1)
            ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
             "* TODO [#B] %?\n  %i\n %U"
             :empty-lines 1)
            ("w" "work" entry (file+headline org-agenda-file-work "Work")
             "* TODO [#A] %?\n  %i\n %U"
             :empty-lines 1) 
            ("p" "Protocol" entry (file+headline org-agenda-file-note "Inbox")
             "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	        ("L" "Protocol Link" entry (file+headline org-agenda-file-note "Inbox")
             "* %? [[%:link][%:description]] \nCaptured On: %U") 
            ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
             "* TODO [#C] %?\n  %i\n %a \n %U"
             :empty-lines 1)
            ))

    (with-eval-after-load 'org-capture
      (defun org-hugo-new-subtree-post-capture-template ()
        "Returns `org-capture' template string for new Hugo post.
        See `org-capture-templates' for more information."
        (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
               (fname (org-hugo-slug title)))
          (mapconcat #'identity
                     `(
                       ,(concat "* TODO " title)
                       ":PROPERTIES:"
                       ,(concat ":EXPORT_FILE_NAME: " fname)
                       ":END:"
                       "\n\n")          ;Place the cursor here finally
                     "\n")))

      (add-to-list 'org-capture-templates
                   '("h"                ;`org-capture' binding + h
                     "Hugo post"
                     entry
                     ;; It is assumed that below file is present in `org-directory'
                     ;; and that it has a "Blog Ideas" heading. It can even be a
                     ;; symlink pointing to the actual location of all-posts.org!
                     (file+headline org-agenda-file-blogposts "Blog Ideas")
                     (function org-hugo-new-subtree-post-capture-template))))

    (setq org-agenda-custom-commands
          '(
            ("w" . "任务安排")
            ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
            ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
            ("b" "Blog" tags-todo "BLOG")
            ("p" . "项目安排")
            ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"work\"")
            ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"zilongshanren\"")
            ("W" "Weekly Review"
             ((stuck "") ;; review stuck projects as designated by org-stuck-projects
              (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
              ))))

    (add-to-list 'org-agenda-custom-commands
                 '("r" "Daily Agenda Review"
                   ((agenda "" ((org-agenda-overriding-header "今日记录")
                                (org-agenda-span 'day)
                                (org-agenda-show-log 'clockcheck)
                                (org-agenda-start-with-log-mode nil)
                                (org-agenda-log-mode-items '(closed clock state))
                                (org-agenda-clockreport-mode t))))))
    
    ))

(provide 'init-org)
