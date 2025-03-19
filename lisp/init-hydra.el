;; init-hydra.el -*- lexical-binding: t -*-

(use-package hydra
  :hook (emacs-lisp-mode . hydra-add-imenu)
  :init
  (setq hydra-hint-display-type 'posframe)
  (with-eval-after-load 'posframe
    (defun hydra-set-posframe-show-params ()
      "Set hydra-posframe style."
      (setq hydra-posframe-show-params
            `(:left-fringe 8
                           :right-fringe 8
                           :internal-border-width 2
                           :internal-border-color ,(face-background 'posframe-border nil t)
                           :background-color ,(face-background 'tooltip nil t)
                           :foreground-color ,(face-foreground 'tooltip nil t)
                           :lines-truncate t
                           :poshandler posframe-poshandler-frame-center-near-bottom)))
    (hydra-set-posframe-show-params)))

(use-package pretty-hydra
  :bind ("<f6>" . toggles-hydra/body)
  :hook (emacs-lisp-mode . (lambda ()
                             (add-to-list
                              'imenu-generic-expression
                              '("Hydras"
                                "^.*(\\(pretty-hydra-define\\) \\([a-zA-Z-]+\\)"
                                2))))
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face 'mode-line-emphasis))
          (height (or height 1.2))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icons-displayable-p) icon-type icon-name)
         (let ((f (intern (format "nerd-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face))))

  (with-no-warnings
    (pretty-hydra-define toggles-hydra (:title (pretty-hydra-title "Toggles" 'faicon "nf-fa-toggle_on")
                                               :color amaranth :quit-key ("q" "C-g"))
                         ("Basic"
                          (("n" (cond ((fboundp 'display-line-numbers-mode)
                                       (display-line-numbers-mode (if display-line-numbers-mode -1 1)))
                                      ((fboundp 'gblobal-linum-mode)
                                       (global-linum-mode (if global-linum-mode -1 1))))
                            "line number"
                            :toggle (or (bound-and-true-p display-line-numbers-mode)
                                        (bound-and-true-p global-linum-mode)))
                           ("a" global-aggressive-indent-mode "aggressive indent" :toggle t)
                           ("d" global-hungry-delete-mode "hungry delete" :toggle t)
                           ("e" electric-pair-mode "electric pair" :toggle t)
                           ("c" flyspell-mode "spell check" :toggle t)
                           ("s" prettify-symbols-mode "pretty symbol" :toggle t)
                           ("l" global-page-break-lines-mode "page break lines" :toggle t)
                           ("b" display-battery-mode "battery" :toggle t)
                           ("i" display-time-mode "time" :toggle t)
                           ("m" doom-modeline-mode "modern mode-line" :toggle t))
                          "Highlight"
                          (("h l" global-hl-line-mode "line" :toggle t)
                           ("h p" show-paren-mode "paren" :toggle t)
                           ("h s" symbol-overlay-mode "symbol" :toggle t)
                           ("h r" rainbow-mode "rainbow" :toggle t)
                           ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
                           ("h i" highlight-indent-guides-mode "indent" :toggle t)
                           ("h t" global-hl-todo-mode "todo" :toggle t))))))

(provide 'init-hydra)
