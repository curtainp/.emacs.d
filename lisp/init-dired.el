;;; init-dired.el -*- lexical-binding: t -*-

(use-package dired
  :straight nil
  :config
  (setq
   ;; Always delete and copy recursively
   dired-recursive-deletes 'top
   dired-recursive-copies 'always
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

  ;;(add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)
  (define-key dired-jump-map (kbd "j") nil)

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
        ("V" . dired-preview-mode))
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

(use-package sudo-edit
  :straight t
  :hook (after-init . sudo-edit-indicator-mode))

(provide 'init-dired)
