;;; -*- lexical-binding: t -*-

(use-package project
  :straight nil
  :bind (:map global-map
              ("C-x p ." . project-dired)
              ("C-x p C-g" . keyboard-quit)
              ("C-x p <return>" . project-dired)
              ("C-x p <delete>" . project-forget-project))
  :custom
  (project-switch-commands
   '((project-find-file "Find file")
     (project-find-regexp "Find regexp")
     (project-find-dir "Find directory")
     (project-dired "Root dired")
     (project-vc-dir "VC-dir")
     (project-shell "Shell")
     (keyboard-quit "Quit")))
  (project-vc-extra-root-markers '(".project"))
  (project-key-prompt-style t))

(use-package diff-mode
  :straight nil
  :config
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  (setq diff-font-lock-prettify t)
  (setq diff-font-lock-syntax nil))

(use-package diff-hl
  :straight t
  :commands turn-on-diff-hl-mode
  :hook ((prog-mode vc-dir-mode) . turn-on-diff-hl-mode)
  :bind (:map global-map
              ("C-x v <up>" . diff-hl-previous-hunk)
              ("C-x v <down>" . diff-hl-next-hunk))
  :config
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  (setq diff-hl-draw-borders nil)
  (setq diff-hl-update-async t))

(use-package ediff
  :straight nil
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t)
  )

(use-package majutsu
  :disabled                             ;; TODO: need to familar with
  :straight (:host github :repo "0WD0/majutsu"))

(with-eval-after-load 'vc
  (require 'vc-annotate)
  (require 'vc-dir)
  (require 'vc-git)
  (require 'add-log)
  (require 'log-view)
  (setq vc-handled-backends '(Git))
  (setq vc-dir-save-some-buffers-on-revert t)
  (setq vc-display-failed-async-commands t)
  (setq vc-find-revision-no-save t)
  (setq add-log-mailing-address "curtainwk@proton.me")
  (setq add-log-keep-changes-together t)
  (setq vc-git-diff-switches '("--patch-with-stat" "--histogram"))
  (setq vc-git-log-switches '("--stat"))
  (setq vc-git-print-log-follow t)
  (setq vc-git-revision-complete-only-branches nil)
  (setq vc-git-root-log-format
        `("%d %h %ai %an: %s"

          ,(concat "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?"
                   "\\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) "
                   "\\(?4:[0-9]\\{4\\}-[0-9-]\\{4\\}[0-9\s+:-]\\{16\\}\\) "
                   "\\(?3:.*?\\):")
          ((1 'log-view-message)
           (2 'change-log-list nil lax)
           (3 'change-log-name)
           (4 'change-log-date))))
  (setq vc-git-log-edit-summary-target-len 50)
  (setq vc-git-log-edit-summary-max-len 70))

(use-package log-edit
  :straight nil
  :hook (log-edit . (log-edit-insert-message-template log-edit-maybe-show-diff))
  :config
  (setq log-edit-confirm 'changed)
  (setq log-edit-keep-buffer nil)
  (setq log-edit-require-final-newline t)
  (setq log-edit-setup-add-author nil))

(use-package magit
  :straight t
  :bind ("C-x g" . magit-status)
  :config
  (setq transient-show-menu 0.5)
  (setq magit-display-buffer-function #'display-buffer)
  (setq magit-define-global-key-bindings nil)
  (setq magit-diff-refine-hunk t)
  (setq magit-diff-refine-ignore-whitespace t)
  (setq magit-log-auto-more t)
  (setq git-commit-summary-max-length 50)
  (setq git-commit-style-convention-checks '(non-empty-second-line))
  (setq git-commit-major-mode #'text-mode))

(use-package forge
  :after magit
  :straight t)

(provide 'init-vc)
