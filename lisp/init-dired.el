;;; init-dired.el -*- lexical-binding: t -*-

(use-package dired
  :straight nil
  :defer
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-after-readin . cw/hide-detail-include-all-subdir-paths))
  :bind (:map dired-mode-map
              ("^" . cw/goto-parent-dir)
              ("i" . dired-hide-details-mode)
              )
  :config
  (setq
   ;; Always delete and copy recursively
   dired-recursive-deletes 'top
   dired-recursive-copies 'always
   delete-by-moving-to-trash t
   ;; Move between two dired buffer quickly
   dired-dwim-target t
   ;; Ask whether destination dirs should get created when copying/removing files.
   dired-create-destination-dirs 'ask
   ;; don't prompt to revert, just do it
   dired-auto-revert-buffer #'dired-buffer-stale-p
   ;; symlink
   dired-hide-details-hide-symlink-targets nil
   dired-auto-revert-buffer #'dired-directory-changed-p
   dired-make-directory-clickable t
   dired-free-space nil
   dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\|svg\\|gif\\|webp\\|heif\\|avif\\)" "swayimg" "xdg-open")
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
     (".*" "xdg-open"))
   dired-mouse-drag-files t)
  (defun cw/hide-detail-include-all-subdir-paths ()
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward dired-subdir-regexp nil t)
        (let* ((match-bounds (cons (match-beginning 1) (match-end 1)))
               (path (file-name-directory (buffer-substring (car match-bounds) (cdr match-bounds))))
               (path-start (car match-bounds))
               (path-end (+ (car match-bounds) (length path)))
               (inhibit-read-only t))
          (put-text-property path-start path-end 'invisible 'dired-hide-details-information)))))

  (defun cw/goto-parent-dir ()
    (interactive)
    (find-file ".."))

  (defun cw/dired-mark-all ()
    (interactive)
    (dired-mark-files-regexp ""))

  (when (eq system-type 'darwin)
    (if (executable-find "gls")
        (setq insert-directory-program "gls") ; Use GNU ls as `gls' from `coreutils' if available.
      ;; Suppress the warning: `ls does not support --dired'.
      (setq dired-use-ls-dired nil)))

  (when (or (not (eq system-type 'darwin)) (executable-find "gls"))
    (setq ls-lisp-use-insert-directory-program t ; Using `insert-directory-program'
          ;; Show directory first
          dired-listing-switches "-Alvh --group-directories-first --time-style=long-iso"))
  )

(use-package nerd-icons-dired
  :disabled
  :straight t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package wdired
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(use-package dired-preview
  :straight t
  :hook (after-init . dired-preview-global-mode)
  :bind
  (:map dired-mode-map
        ("P" . dired-preview-mode))
  :config
  (setq dired-preview-max-size (* (expt 2 20) 10))
  (setq dired-preview-delay 0.5)
  (setq dired-preview-ignored-extensions-regexp
        (concat "\\."
                "\\(gz\\|"
                "zst\\|"
                "tar\\|"
                "xz\\|"
                "rar\\|"
                "zip\\|"
                "iso\\|"
                "epub"
                "\\)")))

(use-package diredfl
  :straight t
  :hook (dired-mode . diredfl-mode)
  :config
  (cl-callf append diredfl-compressed-extensions '(".zst" ".rar" ".7z" ".cab" ".arc" ".zoo")))

(use-package dwim-shell-command
  :straight t
  :defer 30
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command)
         ("C-x C-d" . dwim-shell-command-duplicate))
  :config
  (use-package dwim-shell-commands
    :demand t))

(use-package sudo-edit
  :disabled
  :straight t
  :hook (after-init . sudo-edit-indicator-mode))

(provide 'init-dired)
