;;; -*- lexical-binding: t -*-

(use-package majutsu
  :disabled                             ;; TODO: need to familar with
  :straight (:host github :repo "0WD0/majutsu"))

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


(use-package igist
  :straight (:repo "KarimAziev/igist" :host github)
  :commands (igist-create-new-gist igist-list-gists)
  :custom
  (igist-current-user-name "curtainp")
  (igist-list-format (igist-pick-from-alist
                      '(description
                        public
                        updated_at
                        comments
                        files)
                      (copy-tree igist-default-formats)))
  )             ;; code snippets for Github Gists

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
