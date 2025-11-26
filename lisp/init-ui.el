;; -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(setq
 redisplay-skip-fontification-on-input t)

(setq x-underline-at-descent-line t)

(blink-cursor-mode -1)
(setq global-hl-line-sticky-flag t)
(global-hl-line-mode 1)
(setq indicate-bufferoundaries nil
      indicate-empty-lines nil)

(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

(setq epg-pinentry-mode 'loopback)

(use-package pulsar
  :straight t
  :config
  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10
          pulsar-face 'pulsar-yellow
          pulsar-highlight-face 'pulsar-magenta
          pulsar-region-change-face 'pulsar-red)

  (with-eval-after-load 'evil
    (cl-callf append pulsar-pulse-functions
      '(evil-yank evil-yank-line evil-delete evil-delete-line evil-jump-item
                  evil-paste-after evil-paste-before evil-goto-last-change evil-goto-last-change-reverse)))

  (pulsar-global-mode 1)
  :hook
  ((next-error . (pulsar-pulse-line-red pulsar-recenter-top pulsar-reveal-entry))
   (minibuffer-setup . pulsar-pulse-line-yellow))
  :bind
  ;; pular doesn't define any key bindings.
  (("C-x l" . pulsar-pulse-line) ; override `count-lines-page'
   ("C-x L" . pulsar-highlight-dwim))) ; override `pulsar-highlight-line'

(use-package doom-themes
  :disabled
  :straight t
  :init
  (load-theme 'doom-one t)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package nimbus-theme
  :disabled
  :straight t
  :init
  (load-theme 'nimbus t))

(use-package base16-theme
  :disabled
  :straight t
  :init
  (load-theme 'base16-gruvbox-dark-hard t))


(use-package gruvbox-theme
  :disabled
  :straight t
  :config
  (load-theme 'gruvbox-dark-medium t))

(use-package modus-themes
  :disabled
  :straight t
  :demand t
  :bind (("<f5>" . modus-themes-toggle)
         ("C-<f5>" . modus-themes-select)
         ("M-<f5>" . modus-themes-rotate))
  :config
  (setq modus-themes-custom-auto-reload nil
        modus-themes-to-toggle '(modus-operandi modus-vivendi)
        modus-themes-to-rotate modus-themes-items
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-completions '((t . (bold)))
        modus-themes-prompt '(bold))
  (setq modus-themes-common-palette-overrides nil)
  (modus-themes-load-theme (cadr modus-themes-to-toggle)))

(use-package ef-themes
  :straight (:type git :host github :repo "protesilaos/ef-themes")
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)
  (modus-themes-load-theme 'ef-arbutus))

(use-package fontaine
  :straight t
  :demand t
  :config
  (setq fontaine-latest-state-file
        (locate-user-emacs-file "fontaine-latest-state.eld"))
  (setq fontaine-presets
        '((regular
           :default-height 140
           :default-weight regular
           :fixed-pitch-height 1.0
           :variable-pitch-height 1.0)
          (large
           :default-height 180
           :default-weight normal
           :fixed-pitch-height 1.0
           :variable-pitch-height 1.05)
          (t
           :default-family "JetBrainsMono Nerd Font"
           :fixed-pitch-family "Iosevka Nerd Font"
           :variable-pitch-family "Lato"
           :italic-family "Iosevka Nerd Font"
           :variable-pitch-weight normal
           :bold-weight normal
           :italic-slant italic
           :line-spacing 0.1)))
  (fontaine-set-preset 'regular)
  (set-fontset-font t 'emoji
                    (cond
                     ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
                     ((member "Symbola" (font-family-list)) "Symbola")
                     ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
                     ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
                     ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :family
                (cond
                 ((eq system-type 'darwin)
                  (cond
                   ((member "LXGW WenKai Mono" (font-family-list)) "LXGW WenKai Mono")
                   ((member "PingFang SC" (font-family-list)) "PingFang SC")
                   ((member "WenQuanYi Zen Hei" (font-family-list)) "WenQuanYi Zen Hei")
                   ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
                   ))
                 ((eq system-type 'gnu/linux)
                  (cond
                   ((member "LXGW WenKai Mono" (font-family-list)) "LXGW WenKai Mono")
                   ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei")
                   ((member "WenQuanYi Zen Hei" (font-family-list)) "WenQuanYi Zen Hei")
                   ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
                   ))
                 (t
                  (cond
                   ((member "LXGW WenKai Mono" (font-family-list)) "LXGW WenKai Mono")
                   ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei")
                   ))
                 ))))
  (setq face-font-rescale-alist `(
                                  ("Symbola"             . 1.3)
                                  ("Microsoft YaHei"     . 1.2)
                                  ("WenQuanYi Zen Hei"   . 1.2)
                                  ("LXGW WenKai Mono"    . 1.2)
                                  ("PingFang SC"         . 1.16)
                                  ("Apple Color Emoji"   . 0.91)
                                  ))
  )

(use-package nerd-icons
  :straight t)

(use-package centaur-tabs
  :disabled
  :straight t
  :demand t
  :hook
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  :custom
  (centaur-tabs-icon-type 'nerd-icons)
  (centaur-tabs-set-icons t)
  (centaur-tabs-gray-out-icons 'buffer) ;; gray out icons for buffer that not selected
  (centaur-tabs-set-close-button nil)
  (centaur-tabs-modified-marker t)
  ;; (centaur-tabs-style "rounded")
  (centaur-tabs-left-edge-margin nil)
  :config
  ;; (centaur-tabs-headline-match)
  (centaur-tabs-mode t))


(use-package face-remap
  :straight nil
  :bind
  (("C-x C-=" . global-text-scale-adjust)
   ("C-x C--" . global-text-scale-adjust)
   ("C-x C-0" . global-text-scale-adjust)))

;; Child frame
(use-package posframe
  :disabled
  :hook (after-load-theme . posframe-delete-all)
  :init
  (defface posframe-border
    `((t (:inherit region)))
    "Face used by the `posframe' border."
    :group 'posframe)
  (defvar posframe-border-width 2
    "Default posframe border width.")
  :config
  (with-no-warnings
    (defun my-posframe--prettify-frame (&rest _)
      (set-face-background 'fringe nil posframe--frame))
    (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

    (defun posframe-poshandler-frame-center-near-bottom (info)
      (cons (/ (- (plist-get info :parent-frame-width)
                  (plist-get info :posframe-width))
               2)
            (/ (+ (plist-get info :parent-frame-height)
                  (* 2 (plist-get info :font-height)))
               2)))))

;; Display transient in child frame
(use-package transient-posframe
  :disabled
  :diminish
  :custom-face
  (transient-posframe ((t (:inherit tooltip))))
  (transient-posframe-border ((t (:inherit posframe-border :background unspecified))))
  :hook (after-init . transient-posframe-mode)
  :init (setq transient-mode-line-format nil
              transient-posframe-border-width posframe-border-width
              transient-posframe-poshandler 'posframe-poshandler-frame-center
              transient-posframe-parameters '((left-fringe . 8)
                                              (right-fringe . 8))))

;; [ligature] ligature support for Emacs
(use-package ligature
  :straight t
  :hook ((prog-mode markdown-mode) . ligature-mode)
  :config
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures '(prog-mode markdown-mode org-mode)
                          '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                            ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                            "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                            "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                            "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                            ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                            "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                            "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                            "\\\\" "://"))
  )

(provide 'init-ui)
