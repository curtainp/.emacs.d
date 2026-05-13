;;; init-tools.el -*- lexical-binding: t -*-

(use-package apheleia
  :straight t
  :commands (apheleia-format-buffer)
  :bind
  (:map prog-mode-map
        ("C-c C-f" . apheleia-format-buffer))
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
  (setf (alist-get 'css-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'js-ts-mode apheleia-mode-alist) 'prettier))

(use-package multiple-cursors
  :straight t
  :bind (:map global-map
              ("C-S-c C-S-c" . mc/edit-lines)
              ("C->" . mc/mark-next-like-this)
              ("C-<" . mc/mark-previous-like-this)
              ("C-c C-<" . mc/mark-all-like-this)
              ("C-\"" . mc/skip-to-next-like-this)
              ("C-:" . mc/skip-to-previous-like-this)
              )
  :config
  (setq mc/cmds-to-run-for-all
        '(cw-simple-mark-sexp cw-simple-kill-region-dwim cw-simple-kill-ring-save-dwim)))

(use-package reader
  :straight (:host codeberg :repo "Monadicsheep/emacs-reader"
                   :files (:defaults "render-core.so")
                   :pre-build ("make" "all"))
  :commands (reader-mode))

(use-package insidious
  :disabled
  :straight (:host codeberg :repo "Monadicsheep/insidious"))

(use-package keymap-popup
  :straight (:host codeberg :repo "thanosapollo/emacs-keymap-popup"))

(use-package yeetube
  :straight t
  :bind ("C-c y" . yeetube)
  :config
  (setf yeetube-display-thumbnails-p nil
        yeetube-results-limit 20
        yeetube-enable-tor t))

;; NOTE: need to install TDlib dependency
(use-package telega
  :straight (:host github :repo "zevlg/telega.el"
                   :branch "master"
                   :files (:defaults "contrib" "etc" "server" "Makefile"))
  :commands telega
  :hook (telega-load . (lambda ()
                         (define-key global-map (kbd "C-c t") telega-prefix-map)))
  :bind (
         :map telega-prefix-map
         ("p" . telega-notifications-history)
         )
  :config
  (setq telega-autoplay-mode t)
  (setq telega-notifications-mode t)
  (setq telega-emoji-use-images nil)
  (setq telega-open-file-function 'org-open-file))

(use-package agent-shell
  :straight t
  :defer t
  :config
  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :codex-api-key (lambda ()
                                                                 (let ((json-object-type 'hash-table))
                                                                   (gethash "OPENAI_API_KEY"
                                                                            (json-read-file "~/.codex/auth.json"))))))
  )

(use-package gt
  :straight t
  :commands (gt-translate gt-setup gt-speak)
  :bind
  (:map global-map
        ("C-c g t" . gt-translate)
        ("C-c g s" . gt-speak)
        ("C-c g p" . gt-setup))
  :config
  (setopt gt-langs '(en zh)
          gt-buffer-render-follow-p t
          gt-buffer-render-window-config
          '((display-buffer-reuse-window display-buffer-in-direction)
            (direction . bottom)
            (window-height . 0.4)))
  (setq gt-preset-translators
        `((default . ,(gt-translator
                       :taker (list (gt-taker :pick nil :if 'selection)
                                    (gt-taker :text 'paragraph :if 'read-only)
                                    (gt-taker :text 'word))
                       :engines (list
                                 (gt-stardict-engine :dir "~/.stardict/dic" :dict "朗道英汉字典5.0" :exact t :if 'word)
                                 ;; (gt-youdao-dict-engine)
                                 (gt-youdao-suggest-engine :if '(and word src:en))
                                 (gt-bing-engine :if '(and not-word parts))
                                 ;; TODO: self-host this service
                                 ;; (gt-libre-engine :if 'word)
                                 )
                       :render (list ;; (gt-overlay-render :if 'selection)
                                     ;; (gt-posframe-pop-render :if 'word)
                                     (gt-buffer-render))))
          ;; TODO: add more translators
          ))
  (when (memq system-type '(gnu gnu/linux gnu/kfreebsd))
      (setopt gt-tts-native-engine 'espeak-ng)
      (cl-defmethod gt-speech ((engine (eql 'espeak-ng)) text lang &optional play-fn)
        (let ((command (format "espeak-ng -v %s \"%s\"" lang text)))
          (start-process-shell-command "espeak-ng" nil command)))))

(use-package buffer-to-pdf
  :straight (:host github :repo "protesilaos/buffer-to-pdf")
  :commands (buffer-to-pdf)
  :config
  (setq buffer-to-pdf-directory (expand-file-name "~/Documents/")))

(use-package time
  :straight nil
  :commands (world-clock)
  :config
  (setq display-time-world-list t)
  (setq zoneinfo-style-world-list ; M-x shell RET timedatectl list-timezones
        '(("America/Los_Angeles" "Los Angeles")
          ("America/Vancouver" "Vancouver")
          ("Canada/Pacific" "Canada/Pacific")
          ("America/Chicago" "Chicago")
          ("Brazil/Acre" "Rio Branco")
          ("America/Toronto" "Toronto")
          ("America/New_York" "New York")
          ("Canada/Atlantic" "Canada/Atlantic")
          ("Brazil/East" "Brasília")
          ("UTC" "UTC")
          ("Europe/Lisbon" "Lisbon")
          ("Europe/Brussels" "Brussels")
          ("Europe/Athens" "Athens")
          ("Asia/Riyadh" "Riyadh")
          ("Asia/Tehran" "Tehran")
          ("Asia/Tbilisi" "Tbilisi")
          ("Asia/Yekaterinburg" "Yekaterinburg")
          ("Asia/Kolkata" "Kolkata")
          ("Asia/Singapore" "Singapore")
          ("Asia/Shanghai" "Shanghai")
          ("Asia/Seoul" "Seoul")
          ("Asia/Tokyo" "Tokyo")
          ("Asia/Vladivostok" "Vladivostok")
          ("Australia/Brisbane" "Brisbane")
          ("Australia/Sydney" "Sydney")
          ("Pacific/Auckland" "Auckland")))

  ;; All of the following variables are for Emacs 28
  (setq world-clock-list t)
  (setq world-clock-time-format "%R %z (%Z)	%A %d %B")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  (setq world-clock-timer-enable t)
  (setq world-clock-timer-second 60))

(use-package proced
  :straight nil
  :commands (proced)
  :config
  (setq proced-auto-update-flag 'visible) ; Emacs 30 supports more the `visible' value
  (setq proced-enable-color-flag t) ; Emacs 29
  (setq proced-auto-update-interval 5)
  (setq proced-descend t)
  (setq proced-filter 'user))

(use-package rainbow-delimiters
  :straight t
  :commands rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package symbol-overlay
  :straight t
  :commands symbol-overlay-mode
  :hook ((prog-mode html-mode yaml-mode conf-mode) . symbol-overlay-mode)
  :bind (:map symbol-overlay-mode-map
              ("M-i" . symbol-overlay-put)
              ("M-I" . symbol-overlay-remove-all)
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev)))

(use-package colorful-mode
  :straight t
  :commands (colorful-mode global-colorful-mode)
  :hook (after-init . global-colorful-mode)
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  :config
  (add-to-list 'global-colorful-modes 'helpful-mode))



(provide 'init-tools)
