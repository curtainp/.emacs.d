;;; init-modeline.el -*- lexical-binding: t -*-

(use-package curt-modeline
  :straight nil
  :demand t
  :config
  (setq mode-line-compact nil
        mode-line-right-align-edge 'right-margin)
  (setq-default mode-line-format
                `("%e"
                  curt-modeline-kbd-macro
                  curt-modeline-narrow
                  curt-modeline-buffer-status
                  curt-modeline-window-dedicated-status
                  curt-modeline-input-method
                  "  "
                  curt-modeline-buffer-identification
                  "  "
                  curt-modeline-major-mode
                  curt-modeline-process
                  "  "
                  curt-modeline-vc-branch
                  "  "
                  mode-line-format-right-align
                  "  "
                  curt-modeline-misc-info)))

(use-package awesome-tray
  :disabled
  :straight '(:type git :host github :repo "manateelazycat/awesome-tray")
  :demand t
  :custom
  (awesome-tray-active-modules '("evil" "buffer-name" "location" "belong" "last-command"))
  :config
  (awesome-tray-mode 1))

(use-package doom-modeline
  :disabled
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-time-icon nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-total-line-number t)
  (doom-modeline-enable-word-count t)
  ;; (doom-modeline-hud t)
  ;; (doom-modeline-hud-min-height 1)
  (doom-modeline-continuous-word-count-modes '(markdown-mode markdown-ts-mode gfm-mode org-mode rst-mode latex-mode tex-mode)))

(use-package keycast
  :disabled
  :commands (keycast-doom-modeline-mode)
  :config
  (define-minor-mode keycast-doom-modeline-mode
    "Show keycast in `doom-modeline'."
    :global t
    (if keycast-doom-modeline-mode
        (progn (add-hook 'pre-command-hook 'keycast--update t)
               (add-to-list 'global-mode-string '("" keycast-mode-line " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (cl-callf2 delete '("" keycast-mode-line " ") global-mode-string))))

(provide 'init-modeline)
