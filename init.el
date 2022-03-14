;;; init.el  -*- lexical-binding: t -*-

(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 200000000
                  gc-cons-percentage 0.5
                  read-process-output-max (* 2048 1024))))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)
(toggle-frame-maximized)

;; remove startup message when plugin start
(with-temp-message ""
  (require 'init-package)
  (require 'init-basic)
  (require 'init-completion)
  (require 'init-evil)
  (require 'init-ui)

  ;; lazy load
  (run-with-idle-timer
   1 nil
   #'(lambda ()
       (require 'init-company-lsp)
       (require 'init-treesitter)
       (require 'init-org)
       (require 'init-flycheck)
       (require 'init-ctags)
       (require 'init-vcs)
       (require 'init-projectile)
       (require 'init-language)
       ))
  )
