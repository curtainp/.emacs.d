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
  :hook (after-init . pulsar-global-mode)
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
   ("C-x L" . pulsar-highlight-permanently-dwim))) ; override `pulsar-highlight-line'

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
  (setq modus-themes-variable-pitch-ui t)
  (setq modus-themes-italic-constructs t)
  ;; (setq modus-themes-bold-constructs t)
  (setq modus-themes-headings
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch regular 1.7))
          (3 . (variable-pitch regular 1.6))
          (4 . (variable-pitch regular 1.5))
          (5 . (variable-pitch 1.4))
          (6 . (variable-pitch 1.3))
          (7 . (variable-pitch 1.2))
          (agenda-date . (semilight 1.5))
          (agenda-structure . (variable-pitch light 1.9))
          (t . (variable-pitch 1.1))))
  (modus-themes-load-theme 'ef-dream))  ;; ef-arbutus for light theme

(use-package fontaine
  :straight t
  :commands fontaine-mode
  :config
  (setq fontaine-presets
        '((small
           :default-height 80)
          (regular)
          (medium
           :default-family "Aporetic Serif Mono"
           :default-height 115
           :fixed-pitch-family "Aporetic Serif Mono"
           :variable-pitch-family "Aporetic Sans")
          (large
           :default-height 150)
          (presentation
           :default-height 180)
          (jumbo
           :inherit medium
           :default-height 260)
          (t
           :default-family "Aporetic Sans Mono"
           :default-weight regular
           :default-slant normal
           :default-width normal
           :default-height 100

           :fixed-pitch-family "Aporetic Sans Mono"
           :fixed-pitch-weight nil
           :fixed-pitch-slant nil
           :fixed-pitch-width nil
           :fixed-pitch-height 1.0

           :fixed-pitch-serif-family nil
           :fixed-pitch-serif-weight nil
           :fixed-pitch-serif-slant nil
           :fixed-pitch-serif-width nil
           :fixed-pitch-serif-height 1.0

           :variable-pitch-family "Aporetic Serif"
           :variable-pitch-weight nil
           :variable-pitch-slant nil
           :variable-pitch-width nil
           :variable-pitch-height 1.0

           :mode-line-active-family nil
           :mode-line-active-weight nil
           :mode-line-active-slant nil
           :mode-line-active-width nil
           :mode-line-active-height 1.0

           :mode-line-inactive-family nil
           :mode-line-inactive-weight nil
           :mode-line-inactive-slant nil
           :mode-line-inactive-width nil
           :mode-line-inactive-height 1.0

           :header-line-family nil
           :header-line-weight nil
           :header-line-slant nil
           :header-line-width nil
           :header-line-height 1.0

           :line-number-family nil
           :line-number-weight nil
           :line-number-slant nil
           :line-number-width nil
           :line-number-height 1.0

           :tab-bar-family nil
           :tab-bar-weight nil
           :tab-bar-slant nil
           :tab-bar-width nil
           :tab-bar-height 1.0

           :tab-line-family nil
           :tab-line-weight nil
           :tab-line-slant nil
           :tab-line-width nil
           :tab-line-height 1.0

           :bold-family nil
           :bold-slant nil
           :bold-weight bold
           :bold-width nil
           :bold-height 1.0

           :italic-family nil
           :italic-weight nil
           :italic-slant italic
           :italic-width nil
           :italic-height 1.0
           :line-spacing nil)))
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
                  (fontaine-mode)
                  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
                  ;; Set Symbol Font
                  (cl-loop for font in cw/symbol-fonts
                           when (find-font (font-spec :name font))
                           return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
                  ;; Set Emoji Font
                  (cl-loop for font in cw/emoji-fonts
                           when (find-font (font-spec :name font))
                           return (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))
                  (dolist (charset '(kana han symbol cjk-misc bopomofo))
                    (set-fontset-font (frame-parameter nil 'font) charset
                     (font-spec :family cw/zh-font)))))
  )

(use-package nerd-icons
  :straight t
  :commands nerd-icons-install-fonts)

(use-package doom-modeline
  :disabled
  :straight t
  :commands doom-modeline-mode
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 20)
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

(use-package awesome-tray
  :straight (:host github :repo "manateelazycat/awesome-tray")
  :commands awesome-tray-mode
  :hook (after-init . awesome-tray-mode)
  :custom
  (awesome-tray-file-path-show-filename t)
  (awesome-tray-file-path-truncated-name-length 0)
  (awesome-tray-file-path-full-dirname-levels 0)
  (awesome-tray-active-modules '("location" "belong" "file-path" "mode-name"))
  )


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
