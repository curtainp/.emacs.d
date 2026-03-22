;;; -*- lexical-binding: t -*-

(setq straight-vc-git-default-clone-depth 1)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; PERF: `tty-run-terminal-initialization' is slow
(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
            (defun doom-init-tty-h ()
              (advice-remove #'tty-run-terminal-initialization #'ignore)
              (tty-run-terminal-initialization (selected-frame) nil t))))

(mapc
 (lambda (path)
   (add-to-list 'load-path (locate-user-emacs-file path)))
 '("lisp" "modules"))

(with-temp-message ""
  (require 'init-basic)
  (require 'init-ui)
  (require 'init-completion)
  (require 'init-lsp-bridge)
  (require 'init-nav)
  (require 'init-evil)
  (require 'init-search)
  (require 'init-prog)

  (run-with-idle-timer
   1 nil #'(lambda ()
             (require 'init-dired)
             (require 'init-org)
             (require 'init-latex)
             (require 'init-notes)
             (require 'init-docs)
             (require 'init-tools)
             (require 'init-rime)
             (require 'init-vc)
             ))
  )
