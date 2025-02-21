;;; -*- lexical-binding: t -*-

(defconst cs/fallback-fonts '("Jigmo" "Jigmo2" "Jigmo3"))
(defconst cs/emoji-fonts '("Apple Color Emoji"
			   "Noto Color Emoji"
			   "Noto Emoji"
			   "Segoe UI Emoji"
			   "Symbola"))
(defconst cs/default-font "Iosevka 18")
(defconst cs/zh-default-font "LXGW WenKai")
(defconst cs/symbol-default-font "Symbols Nerd Font Mono")

(setq
 idle-update-delay 1.0
 redisplay-skip-fontification-on-input t
 highlight-nonselected-windows nil
 cursor-in-non-selected-windows nil
 inhibit-compacting-font-caches t)

(blink-cursor-mode -1)
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

(define-fringe-bitmap 'right-curly-arrow
  [#b00110000
   #b00110000
   #b00000000
   #b00110000
   #b00110000
   #b00000000
   #b00110000
   #b00110000])
(define-fringe-bitmap 'left-curly-arrow
  [#b00110000
   #b00110000
   #b00000000
   #b00110000
   #b00110000
   #b00000000
   #b00110000
   #b00110000])
(define-fringe-bitmap 'right-arrow
  [#b00000000
   #b00000000
   #b00001110
   #b00001110
   #b00001110
   #b00000000
   #b00000000
   #b00000000])
(define-fringe-bitmap 'left-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b01110000
   #b01110000
   #b01110000
   #b00000000
   #b00000000])

(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

(setq use-file-dialog nil
      use-dialog-box nil)

(setq epg-pinentry-mode 'loopback)

(defun +setup-fonts ()
  "Setup fonts."
  ;; Setting the default
  (set-face-attribute 'default nil :font cs/default-font :weight 'normal)
  ;; 特殊字符需要安装 Symbola 字体
  ;; https://www.wfonts.com/font/symbola
  ;; "Emacs 28 now has 'emoji . before, emoji is part of 'symbol"
  ;; 根据上面这句话应该写成 'emoji 就可以了，但是由于 Emoji 本身
  ;; 分布比较散，所以还是先设置 'unicode 后再设置 CJK 比较靠谱。
  ;; 特例：'emoji 就会导致 ⛈️ fallback 到 ⛈
  ;; https://emacs-china.org/t/emacs/15676/34
  (cl-loop for font in cs/emoji-fonts
           when (find-font (font-spec :name font))
           return (set-fontset-font
                   t
                   'unicode
                   (font-spec :family font
                              :size
                              (cond ((eq system-type 'darwin) 12)
                                    ((eq system-type 'gnu/linux) 12)))
                   nil 'prepend))
  ;; Set Chinese font
  ;; Do not use 'unicode charset, it will cause the English font setting invalid
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family cs/zh-default-font)))
  ;; Setting fall-back fonts
  ;; https://idiocy.org/emacs-fonts-and-fontsets.html
  (dolist (font cs/fallback-fonts)
    (when (member font (font-family-list))
      (set-fontset-font "fontset-default" 'han font nil 'append)))
  ;; Force Emacs to search by using font-spec
  (set-fontset-font t 'han (font-spec :script 'han) nil 'append)
  (set-fontset-font t '(#xE000 . #xF8FF) cs/symbol-default-font))

(+setup-fonts)


;; Smooth Scroll (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll) ((control) . nil))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))

(setq custom-safe-themes t)

(use-package doom-themes
  :straight t
  :demand t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (load-theme 'doom-one t)
  ;; (doom-themes-visual-bell-config)
  (with-eval-after-load 'org
    (doom-themes-org-config)))

(use-package modus-themes
  :straight t)

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-time-icon nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-total-line-number t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes '(markdown-mode markdown-ts-mode gfm-mode org-mode rst-mode latex-mode tex-mode)))

(use-package keycast
  :straight t
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

;; [window-divider] Display window divider
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

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

(setq frame-title-format
      '((:eval (or buffer-file-truename "%b"))
        (" · Emacs")))

(provide 'init-ui)
