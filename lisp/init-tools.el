;;; lexical-binding: t -*-

(use-package ialign
  :disabled
  :bind (("C-x l" . ialign)))

;;(use-package symbol-overlay)

(use-package sort-tab
  :disabled
  :straight '(:type git :host github :repo "manateelazycat/sort-tab")
  ;; :demand t
  :config
  (sort-tab-mode))

(use-package holo-layer
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

;; [project] Project manager
(use-package project
  :straight nil
  :bind (:map project-prefix-map
              ("m" . magit-status))
  :config
  (setq project-switch-commands '((project-find-file "File")
                                  (project-find-regexp "Regexp")
                                  (project-switch-to-buffer "Buffer")
                                  (project-dired "Dired")
                                  (project-eshell "Eshell")
                                  (project-search "Search")
                                  (magit-status "Magit")))

  (defun +project-previous-buffer (arg)
    "Toggle to the previous buffer that belongs to current project."
    (interactive "P")
    (unless arg
      (if-let* ((pr (project-current)))
          (switch-to-buffer
           (->> (project--buffer-list pr)
                (--remove (or (minibufferp it)
                              (get-buffer-window-list it)))
                (car))))))

  ;; Use [fd] to find file in project
  (defun +search-project-files-with-fd (dir)
    "Use `fd' to list files in DIR."
    (let* ((default-directory dir)
           (localdir (file-local-name (expand-file-name dir)))
           (command (format "fd -H -t f -0 . %s" localdir)))
      (project--remote-file-names
       (sort (split-string (shell-command-to-string command) "\0" t)
             #'string<))))
  (cl-defmethod project-files ((project (head local)) &optional dirs)
    "Override `project-files' to use `fd' in local projects."
    (mapcan #'+search-project-files-with-fd
            (or dirs (list (project-root project)))))
  )

(use-package vterm
  :disabled
  :hook (vterm-mode . compilation-shell-minor-mode)
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
