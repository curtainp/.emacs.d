;;; -*- lexical-binding: t -*-

;; startup time hook
(defun cw/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%0.3f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'cw/display-startup-time)

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent ; Emacs 28 with native compilation
        native-compile-prune-cache t
        native-comp-jit-compilation t))

;; Enable default disabled command
(mapc
 (lambda (command)
   (put command 'disabled nil))
 '(list-timers list-threads narrow-to-page narrow-to-defun narrow-to-region upcase-region downcase-region))

;; Disable default enabled command
(mapc
 (lambda (command)
   (put command 'disabled t))
 '(eshell project-eshell overwrite-mode iconify-frame diary))

(mapc
 (lambda (path)
   (add-to-list 'load-path (locate-user-emacs-file path)))
 '("lisp" "modules"))

;;(require 'init-package)
(with-temp-message ""
  (require 'init-straight)
  (require 'init-basic)
  (require 'init-ui)
  (require 'init-modeline)
  (require 'init-completion)
  (require 'init-lsp-bridge)
  (require 'init-nav)
  (require 'init-evil)
  ;;(require 'init-search)
  (require 'init-prog)
  ;;(require 'init-emigo)

  (run-with-idle-timer
   1 nil #'(lambda ()
             ;; (require 'init-popweb)
             ;; (require 'init-eaf)
             ;; (require 'init-llm)
             (require 'init-dired)
             (require 'init-org)
             (require 'init-notes)
             (require 'init-docs)
             (require 'init-tools)
             (require 'init-rime)
             (require 'init-vc)
             ))
  )
