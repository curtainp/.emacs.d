;; -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(global-hl-line-mode 1)

(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

(use-package pulsar
  :straight t
  :commands pulsar-global-mode
  :init
  (pulsar-global-mode 1)
  :config
  (setq pulsar-delay 0.055
        pulsar-iterations 10
        pulsar-face 'pulsar-yellow
        pulsar-highlight-face 'pulsar-magenta
        pulsar-region-change-face 'pulsar-red)

  (with-eval-after-load 'evil
    (cl-callf append pulsar-pulse-functions
      '(evil-yank evil-yank-line evil-delete evil-delete-line evil-jump-item
                  evil-paste-after evil-paste-before evil-goto-last-change evil-goto-last-change-reverse)))

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

(use-package ef-themes
  :straight (:type git :host github :repo "protesilaos/ef-themes")
  :commands ef-themes-take-over-modus-themes-mode
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)
  (modus-themes-load-theme 'ef-dream))  ;; ef-arbutus for light theme

(use-package fontaine
  :straight t
  :config
  (setq fontaine-latest-state-file
        (locate-user-emacs-file "fontaine-latest-state.eld"))
  (setq fontaine-presets
        '((regular
           :default-height 140
           :default-weight regular
           :fixed-pitch-height 1.2
           :variable-pitch-height 1.2)
          (large
           :default-height 200
           :default-weight normal
           :fixed-pitch-height 1.0
           :variable-pitch-height 1.05)
          (t
           :default-family "Iosevka Term"
           :fixed-pitch-family "Iosevka Term"
           ;; :variable-pitch-family "Roboto"
           ;; :variable-pitch-family "Georgia"
           :variable-pitch-family "Lato"
           :line-spacing 0.1)))
  (setq face-font-rescale-alist `(
                                  ("Symbola"             . 1.3)
                                  ("Microsoft YaHei"     . 1.2)
                                  ("WenQuanYi Zen Hei"   . 1.2)
                                  ("LXGW WenKai Mono"    . 1.2)
                                  ("PingFang SC"         . 1.16)
                                  ("Apple Color Emoji"   . 0.91)
                                  ))
  :hook
  (after-init . (lambda ()
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
                                   ((member "LXGW WenKai" (font-family-list)) "LXGW WenKai")
                                   ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei")
                                   ((member "WenQuanYi Zen Hei" (font-family-list)) "WenQuanYi Zen Hei")
                                   ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
                                   ))
                                 (t
                                  (cond
                                   ((member "LXGW WenKai Mono" (font-family-list)) "LXGW WenKai Mono")
                                   ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei")
                                   ))
                                 ))))))
  )

(use-package nerd-icons
  :straight t
  :config
  (when (not (find-font (font-spec :name nerd-icons-font-family)))
    (nerd-icons-install-fonts t)))

(use-package doom-modeline
  :straight t
  :commands doom-modeline-mode
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-irc nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-gnus nil)
  (doom-modeline-github nil)
  (doom-modeline-buffer-file-name-style 'truncate-upto-root)
  (doom-modeline-persp-name nil)
  ; (doom-modeline-time-icon nil)
  ; (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-unicode-fallback t)
  ; (doom-modeline-total-line-number t)
  (doom-modeline-enable-word-count nil)
  ;; (doom-modeline-hud t)
  ;; (doom-modeline-hud-min-height 1)
  ; (doom-modeline-continuous-word-count-modes '(markdown-mode markdown-ts-mode gfm-mode org-mode rst-mode latex-mode tex-mode))
  )

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

(use-package default-text-scale
  :straight t
  :hook (after-init . default-text-scale-mode))

(use-package ligature
  :straight t
  :commands ligature-mode
  :hook (prog-mode . ligature-mode)
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
