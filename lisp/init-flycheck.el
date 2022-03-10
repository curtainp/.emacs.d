;;; init-flycheck.el -*- lexical-binding: t -*-

(use-package flycheck
  :diminish
  :commands flycheck-redefine-standard-error-levels
  :hook (after-init . global-flycheck-mode)
  :init
  (setq flycheck-global-modes
        '(not text-mode outline-mode fundamental-mode lisp-interaction-mode
              org-mode diff-mode shell-mode eshell-mode term-mode vterm-mode)
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-indication-mode (if (display-graphic-p)
                                     'right-fringe
                                   'right-margin)
        ;; only check when saving and mode enable
        flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  ;; (flycheck-redefine-standard-error-levels "⏴" 'flycheck-fringe-bitmap-arrow)
  (use-package flycheck-posframe
    :custom-face
    (flycheck-posframe-face ((t (:foreground ,(face-foreground 'success)))))
    (flycheck-posframe-info-face ((t (:foreground ,(face-foreground 'success)))))
    (flycheck-posframe-background-face ((t (:inherit tooltip))))
    (flycheck-posframe-border-face ((t (:inherit font-lock-comment-face))))
    :hook (flycheck-mode . flycheck-posframe-mode)
    :init
    (setq flycheck-posframe-border-width 1)
    (add-hook 'flycheck-posframe-inhibit-functions
              (lambda (&rest _) (bound-and-true-p company-backend)))
    :config
    (with-no-warnings
      (defun my-flycheck-posframe-show-posframe (errors)
        "Display ERRORS, using posframe.el library."
        (posframe-hide flycheck-posframe-buffer)
        (when (and errors
                   (not (run-hook-with-args-until-success 'flycheck-posframe-inhibit-functions)))
          (let ((poshandler (intern (format "posframe-poshandler-%s" flycheck-posframe-position)))
                (str (flycheck-posframe-format-errors errors)))
            (unless (functionp poshandler)
              (setq poshandler nil))
            (flycheck-posframe-check-position)
            (posframe-show
             flycheck-posframe-buffer
             :string (concat (propertize "\n" 'face '(:height 0.3))
                             str
                             (propertize "\n\n" 'face '(:height 0.3)))
             :background-color (face-background 'flycheck-posframe-background-face nil t)
             :position (point)
             :left-fringe 8
             :right-fringe 8
             :max-width (round (* (frame-width) 0.62))
             :max-height (round (* (frame-height) 0.62))
             :internal-border-width flycheck-posframe-border-width
             :internal-border-color (face-foreground 'flycheck-posframe-border-face nil t)
             :poshandler poshandler
             :hidehandler #'flycheck-posframe-hidehandler))))
      (advice-add #'flycheck-posframe-show-posframe :override #'my-flycheck-posframe-show-posframe))
    )
  )


(provide 'init-flycheck)
