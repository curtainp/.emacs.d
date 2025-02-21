;;; lexical-binding: t -*-

(use-package ialign
  :straight t
  :bind (("C-x l" . ialign)))

(use-package symbol-overlay
  :straight t)

;; [project] Project manager
(use-package project
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
      (if-let ((pr (project-current)))
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
  :straight t
  :hook (vterm-mode . compilation-shell-minor-mode)
  :bind (:map vterm-mode-map ([return] . vterm-send-return))
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 10000))

(use-package multi-vterm
  :straight t
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
