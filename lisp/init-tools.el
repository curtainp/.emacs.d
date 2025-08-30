;;; init-tools.el -*- lexical-binding: t -*-

(use-package ialign
  :disabled
  :bind (("C-x l" . ialign)))

(use-package smartparens
  :straight t
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config))

(use-package gitmoji
  :straight (:host github :repo "Spike-Leung/gitmoji")
  :hook (git-commit-mode . gitmoji-commit-mode)
  :config
  (setq gitmoji-selection-backend '(consult))
  (setq gitmoji--insert-utf8-emoji t)
  (setq gitmoji--display-utf8-emoji t))

(use-package list-unicode-display
  :straight t)

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
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package symbol-overlay
  :straight t
  :hook ((prog-mode html-mode yaml-mode conf-mode) . symbol-overlay-mode)
  :bind (:map symbol-overlay-mode-map
              ("M-i" . symbol-overlay-put)
              ("M-I" . symbol-overlay-remove-all)
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev)))

(use-package rainbow-mode
  :straight t
  :hook ((css-mode html-mode sass-mode) . rainbow-mode)
  :init
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil)
  :bind (:map ctl-x-x-map
              ("c" . rainbow-mode)))

(use-package sort-tab
  :disabled
  :straight '(:type git :host github :repo "manateelazycat/sort-tab")
  ;; :demand t
  :config
  (sort-tab-mode))

(use-package holo-layer
  :disabled
  :straight '(:type git :host github :repo "manateelazycat/holo-layer"
                    :files (:defaults "*.el" "*.py" "icon_cache" "plugin" "resources")
                    :build (:not compile))
  :demand t
  :custom
  (holo-layer-enable-cursor-animation nil)
  (holo-layer-enable-window-border nil)
  (holo-layer-sort-tab-ui nil)
  (holo-layer-hide-mode-line t)
  :config
  (holo-layer-enable))


(use-package hungry-delete
  :disabled
  :straight t
  :hook (after-init . global-hungry-delete-mode)
  :init (setq hungry-delete-chars-to-skip " \t\f\v"
              hungry-delete-except-modes
              '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))

(use-package eee
  :disabled
  :bind-keymap
  ("C-c e" . ee-keymap)
  :config
  (setq ee-terminal-command "st"))

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

(use-package projectile
  :straight t
  :config
  (projectile-mode +1))

(use-package vterm-toggle
  :disabled
  :straight t
  :bind (:map global-map
              ([f8] . vterm-toggle)
              :map vterm-mode-map
              ([f8] . vterm-toggle))
  )


(use-package vterm
  :disabled
  :straight t
  :bind (:map vterm-mode-map ([return] . vterm-send-return))
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 10000))

(use-package multi-vterm
  :disabled
  :bind (([remap project-shell] . multi-vterm-project)
         ([f1] . +multi-vterm-dedicated-toggle-dwim)
         :map vterm-mode-map ([f1] . +multi-vterm-dedicated-toggle-dwim))
  :custom
  (multi-vterm-dedicated-window-height-percent 30)
  :config
  (defun +multi-vterm-dedicated-toggle-dwim ()
    "Toggle the vterm window.
When in a project, toggle a `multi-vterm-project' terminal. When outside
a project, call `multi-vterm-dedicated-toggle'."
    (interactive)
    (if-let* ((buf-name (and (multi-vterm-project-root) (multi-vterm-project-get-buffer-name)))
              (display-buffer-alist (cons `(,(regexp-quote buf-name)
                                            (display-buffer-reuse-window display-buffer-at-bottom)
                                            (dedicated . t) ;; Close when finished
                                            (window-height . 0.3))
                                          display-buffer-alist)))
        (if-let* ((buf (get-buffer buf-name))
                  ((buffer-live-p buf)))
            (if-let* ((win (get-buffer-window buf))) ; The project's vterm already exists, toggle it's window
                (delete-window win)
              (pop-to-buffer buf))
          (multi-vterm-project))
      (multi-vterm-dedicated-toggle))))


(provide 'init-tools)
