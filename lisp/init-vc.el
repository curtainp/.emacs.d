;;; -*- lexical-binding: t -*-

(use-package magit
  :straight t
  :custom
  (magit-diff-refine-hunk t)
  (magit-revision-show-gravatars t)
  (magit-save-repository-buffers nil)
  (magit-format-file-function #'magit-format-file-nerd-icons)
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1) ; Show in new window
  :init
  ;; Replace the `project-vc-dir' by `magit-project-status' in project prefix and switch commands
  (with-eval-after-load 'project
    (keymap-set project-prefix-map "v" 'magit-project-status)
    (when-let* ((vc (assoc 'project-vc-dir project-switch-commands)))
      (setcar vc 'magit-project-status)
      (setcdr vc '("Magit project status"))))
  :bind (:map global-map
              ("C-x g" . magit-status))
  :config
  ;; Automatically refresh Magit after save
  (add-hook 'after-save-hook 'magit-after-save-refresh-status))

(use-package git-commit
  :straight nil
  :after magit
  :commands (global-git-commit-mode)
  :custom
  (git-commit-summary-max-length 80) ; defaults to Github's max commit message length
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  :init
  (global-git-commit-mode 1))

;; Show source files' TODOs (and FIXMEs, etc) in Magit status buffer
(use-package magit-todos
  :straight t)

;; Magit extension for "git-imerge"
(use-package magit-imerge
  :straight t
  :after magit
  :init
  (with-eval-after-load 'transient
    (transient-append-suffix 'magit-merge "m" '("M" "magit-imerge" magit-imerge))))

;; View diffs side-by-side in Emacs
(use-package diffview
  :disabled
  :straight t)

;; A structural diff that understands syntax
(use-package difftastic
  :disabled
  :straight t)


(provide 'init-vc)
