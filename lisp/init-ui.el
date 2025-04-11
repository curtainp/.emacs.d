;; -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(defconst cs/fallback-fonts '("Jigmo" "Jigmo2" "Jigmo3"))
(defconst cs/emoji-fonts '(
               "Apple Color Emoji"
			   "Noto Color Emoji"
			   "Noto Emoji"
			   "Segoe UI Emoji"
               "Symbola"
			   ))
(defconst cs/default-font "JetBrainsMono 12")
(defconst cs/zh-default-font "LXGW WenKai")
(defconst cs/symbol-default-font "Symbols Nerd Font Mono")

(setq
 redisplay-skip-fontification-on-input t)

(setq x-underline-at-descent-line t)

(blink-cursor-mode -1)
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

(setq epg-pinentry-mode 'loopback)

(defun +setup-fonts ()
  "Setup fonts."
  ;; Setting the default
  (set-face-attribute 'default nil :font cs/default-font :weight 'normal)
  ;; 特殊字符需要安装 Symbola 字体 😇
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
                                    ((eq system-type 'gnu/linux) 25)))
                   nil 'prepend))
  ;; Set Chinese font
  ;; Do not use 'unicode charset, it will cause the English font setting invalid
  (when (display-graphic-p)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family cs/zh-default-font))))
  ;; Setting fall-back fonts
  ;; https://idiocy.org/emacs-fonts-and-fontsets.html
  (dolist (font cs/fallback-fonts)
    (when (member font (font-family-list))
      (set-fontset-font "fontset-default" 'han font nil 'append)))
  ;; Force Emacs to search by using font-spec
  (set-fontset-font t 'han (font-spec :script 'han) nil 'append)
  (set-fontset-font t '(#xE000 . #xF8FF) cs/symbol-default-font))

(add-hook 'window-setup-hook '+setup-fonts)
(add-hook 'server-after-make-frame-hook '+setup-fonts)

;; Smooth Scroll (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll) ((control) . nil))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))

(use-package pulsar
  :straight t
  :config
  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10
          pulsar-face 'pulsar-yellow
          pulsar-highlight-face 'pulsar-magenta)

  (pulsar-global-mode 1)
  :hook
  ((next-error . (pulsar-pulse-line-red pulsar-recenter-top pulsar-reveal-entry))
   (minibuffer-setup . pulsar-pulse-line-yellow)
   )
  :bind
  ;; pular doesn't define any key bindings.
  (("C-x l" . pulsar-pulse-line) ; override `count-lines-page'
   ("C-x L" . pulsar-highlight-dwim))) ; override `pulsar-highlight-line'

(use-package doom-themes
  :straight t
  :init
  (load-theme 'doom-one t)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; (doom-themes-visual-bell-config)
  (with-eval-after-load 'org
    (doom-themes-org-config)))

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

(use-package awesome-tray
  :straight '(:type git :host github :repo "manateelazycat/awesome-tray")
  :demand t
  :custom
  (awesome-tray-active-modules '("evil" "buffer-name" "location" "belong" "last-command"))
  :config
  (awesome-tray-mode 1))

(use-package nerd-icons
  :straight t)

(use-package centaur-tabs
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
  :config
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t))

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

;; [window-divider] Display window divider
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

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

;; (setq frame-title-format
;;       '((:eval (or buffer-file-truename "%b"))
;;         (" · Emacs")))

(provide 'init-ui)
