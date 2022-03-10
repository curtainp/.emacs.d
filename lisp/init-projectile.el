;;;  init-projectile.el -*- lexical-binding: t -*-

(use-package projectile
  :diminish
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf)
  :config
  ;; use `rg` for find files
  (setq projectile-generic-command
        (let ((rg-cmd ""))
          (dolist (dir projectile-globally-ignored-directories)
            (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
          (concat "rg -0 --files --color=never --hidden" rg-cmd)))
  )

(provide 'init-projectile)
