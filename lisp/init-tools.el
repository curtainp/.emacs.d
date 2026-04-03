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

(use-package agent-shell
  :straight t
  :defer t
  :config
  ;; Evil integration: insert = newline, normal = send
  (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
  (evil-define-key 'normal agent-shell-mode-map
    (kbd "RET") #'comint-send-input
    "q" #'quit-window
    "gj" #'agent-shell-next-item
    "gk" #'agent-shell-previous-item)
  ;; diff buffers use emacs state (they have their own accept/reject keybindings)
  (add-hook 'agent-shell-diff-mode-hook #'evil-emacs-state))

(use-package gt
  :straight t
  :commands (gt-translate gt-setup gt-speak)
  ;; integration with evil
  :hook (gt-buffer-render-init . (lambda ()
                                   (define-key evil-normal-state-local-map (kbd "q") 'quit-window)))
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
                                    (gt-taker :text 'paragraph :if '(Info-mode help-mode helpful-mode elfeed-show-mode))
                                    (gt-taker :text 'word))
                       :engines (list (gt-youdao-dict-engine)
                                      (gt-stardict-engine :dir "~/.stardict/dic" :dict "朗道英汉字典5.0" :exact t))
                       :render (list (gt-overlay-render :if '(Info-mode help-mode helpful-mode elfeed-show-mode))
                                     (gt-buffer-render))))
          ;; TODO: add more translators
          ))
  (when (memq system-type '(gnu gnu/linux gnu/kfreebsd))
      (setopt gt-tts-native-engine 'espeak-ng)
      (cl-defmethod gt-speech ((engine (eql 'espeak-ng)) text lang &optional play-fn)
        (let ((command (format "espeak-ng -v %s \"%s\"" lang text)))
          (start-process-shell-command "espeak-ng" nil command)))))

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

(use-package sort-tab
  :disabled
  :straight '(:type git :host github :repo "manateelazycat/sort-tab")
  ;; :demand t
  :config
  (sort-tab-mode))


(use-package multiple-cursors
  :disabled
  :bind (("C-c m" . multiple-cursors-hydra/body)
         ("C-S-c C-S-c"   . mc/edit-lines)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("s-<mouse-1>"   . mc/add-cursor-on-click)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space))
  :pretty-hydra
  ((:title (pretty-hydra-title "Multiple Cursors" 'mdicon "nf-md-cursor_move")
    :color amaranth :quit-key ("q" "C-g"))
   ("Up"
	(("p" mc/mark-previous-like-this "prev")
	 ("P" mc/skip-to-previous-like-this "skip")
	 ("M-p" mc/unmark-previous-like-this "unmark")
	 ("|" mc/vertical-align "align with input CHAR"))
    "Down"
    (("n" mc/mark-next-like-this "next")
	 ("N" mc/skip-to-next-like-this "skip")
	 ("M-n" mc/unmark-next-like-this "unmark"))
    "Misc"
    (("l" mc/edit-lines "edit lines" :exit t)
	 ("a" mc/mark-all-like-this "mark all" :exit t)
	 ("s" mc/mark-all-in-region-regexp "search" :exit t)
     ("<mouse-1>" mc/add-cursor-on-click "click"))
    "% 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")"
	(("0" mc/insert-numbers "insert numbers" :exit t)
	 ("A" mc/insert-letters "insert letters" :exit t)))))


(provide 'init-tools)
