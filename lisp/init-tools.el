;;; init-tools.el -*- lexical-binding: t -*-

(use-package agent-shell
  :straight t
  :commands (agent-shell)
  :preface
  (defun +agent-shell-setup-completion ()
    "Use Corfu for agent-shell candidates without completing ordinary prose."
    (require 'corfu)
    ;; The viewport inherits text-mode, which may already enable Corfu.
    ;; Restart it so a previous automatic-completion hook is removed.
    (corfu-mode -1)
    (setq-local corfu-auto nil
                corfu-preselect 'first
                corfu-on-exact-match nil
                corfu-map (copy-keymap corfu-map))
    ;; Only the active popup owns these keys.  Outside it, keep the
    ;; shell's RET submission and TAB item navigation unchanged.
    (dolist (key '("TAB" "<tab>" "RET" "<return>"))
      (keymap-set corfu-map key #'corfu-insert))
    (corfu-mode 1))
  :hook ((agent-shell-mode agent-shell-viewport-edit-mode)
         . +agent-shell-setup-completion)
  :config
  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :login t))
  (setq agent-shell-antigravity-authentication
        (agent-shell-antigravity-make-authentication :login t))
  ;; Resolve the local installation independently of Emacs' exec-path.
  (let ((server (expand-file-name "~/.local/bin/agy_acp_server.par")))
    (when (file-executable-p server)
      (setq agent-shell-antigravity-acp-command
            (cons (file-truename server)
                  (when (eq system-type 'gnu/linux) '("--uid="))))
      ;; The standalone server needs an explicit path to its companion.
      (let ((harness (expand-file-name
                      "localharness_external"
                      (file-name-directory (file-truename server)))))
        (when (file-executable-p harness)
          (let ((process-environment agent-shell-antigravity-environment))
            (setenv "ANTIGRAVITY_HARNESS_PATH" harness)
            (setq agent-shell-antigravity-environment process-environment))))))
  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :login t))
  )

(use-package apheleia
  :straight t
  :commands (apheleia-format-buffer)
  :bind
  (:map prog-mode-map
        ("C-c C-f" . apheleia-format-buffer))
  :config
  (setf (alist-get 'rustfmt apheleia-formatters)
      '("apheleia-from-project-root" "Cargo.lock" "cargo" "--quiet" "fmt" "--" "--quiet" "--emit" "stdout"))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
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

(use-package excali
  :straight (:host github :repo "yibie/excali-mode"
                   :files (:defaults "excali-module.so")
                   :pre-build (("make") ("make" "fonts")))
  :commands (excali-open excali-new))

;; NOTE: need to install TDlib dependency
(use-package telega
  :straight (:host github :repo "zevlg/telega.el"
                   :branch "master"
                   :files (:defaults "contrib" "etc" "server" "Makefile"))
  :commands telega
  :hook (telega-load . telega-notifications-mode)
  :bind (:map telega-prefix-map
         ("p" . telega-notifications-history)
         )
  :bind-keymap
  ("C-c t" . telega-prefix-map)
  :config
  (setq telega-autoplay-mode t)
  (setq telega-open-file-function 'org-open-file)
  (setq telega-sticker--use-thumbnail t
        telega-symbols-emojify nil))

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
                                    (gt-taker :text 'paragraph)
                                    (gt-taker :text 'word))
                       :engines (list
                                 (gt-stardict-engine :dir "~/.stardict/dic" :dict "朗道英汉字典5.0" :exact t :if 'word)
                                 ;; (gt-youdao-dict-engine)
                                 (gt-youdao-suggest-engine :if '(and word src:en))
                                 (gt-bing-engine :if 'not-word)
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
  :disabled
  :straight (:host github :repo "protesilaos/buffer-to-pdf")
  :commands (buffer-to-pdf)
  :bind (:map global-map
              ("C-c x p" . buffer-to-pdf))
  :config
  (setq buffer-to-pdf-directory (expand-file-name "~/Documents/")))

(use-package shr-tag-pre-highlight
  :straight t
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

(use-package elfeed
  :straight t
  :commands (elfeed)
  :bind (:map global-map
              ("C-c x e" . elfeed)))

(use-package elfeed-org
  :straight t
  :commands elfeed-org
  :hook (after-init . elfeed-org))

(use-package etaf
  :straight t
  :commands etaf-install
  :config
  (call-interactively #'etaf-install))

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

(use-package winner
  :straight nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :config
  (defun toggle-delete-other-windows ()
    "Delete other windows in frame if any, or restore previous window layout."
    (interactive)
    (if (and winner-mode
             (equal (selected-window) (next-window)))
        (winner-undo)
      (delete-other-windows)))
  (global-set-key (kbd "C-x 1") #'toggle-delete-other-windows)
  :custom
  (winner-boring-buffers
   '("*Completions*" "*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
     "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
     "*esh command on file*")))

(use-package popper
  :disabled
  :straight t
  :commands popper-mode
  :hook (emacs-startup . popper-mode)
  :bind (:map global-map
              ("C-M-<tab>" . popper-cycle)
              ("M-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
     "\\*Async Shell Command\\*"
     compilation-mode
     "\\*compilation\\*"
     "\\*Telega User\\*"
     "\\*Telegram Chat Info\\*"
     "\\*Telegram Message Info\\*"
     "\\*Telegram Sticker Set\\*"
     "\\*Embark Actions\\*"
     helpful-mode
     "\\*ghostel\\*" ghostel-mode
     "\\*Org Select\\*"
     ))
  :config
  (popper-echo-mode 1)
  ;; (popper-tab-line-mode 1)
  (defun +popper-close-window-hack (&rest _)
    "Close popper window via `C-g'."
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))
  (advice-add 'keyboard-quit :before #'+popper-close-window-hack)
  )

(provide 'init-tools)
