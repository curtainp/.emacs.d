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

(defun cw/macos-transparent-frame ()    ;; this functionality need frame-transparency patch for emacs-31
  (progn
    (set-frame-parameter nil 'ns-alpha-elements '(ns-alpha-all))
    (set-frame-parameter nil 'alpha-background 0.5)
    (set-frame-parameter nil 'ns-background-blur 20)
    ))

(when (and (eq system-type 'darwin) (display-graphic-p))
  (add-hook 'emacs-startup-hook #'cw/macos-transparent-frame))

(add-hook 'emacs-startup-hook #'cw/display-startup-time)

;; Don't show logging level beyond :emergency, but also record with warnings buffer
(setq warning-minimum-level :emergency)

;; PERF: `tty-run-terminal-initialization' is slow
(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
            (defun doom-init-tty-h ()
              (advice-remove #'tty-run-terminal-initialization #'ignore)
              (tty-run-terminal-initialization (selected-frame) nil t))))

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent ; Emacs 28 with native compilation
        native-compile-prune-cache t
        native-comp-jit-compilation t))

(setq custom-file (locate-user-emacs-file "custom.el"))

(mapc
 (lambda (path)
   (add-to-list 'load-path (locate-user-emacs-file path)))
 '("lisp" "modules"))

(with-temp-message ""
  (require 'init-straight)
  (require 'init-basic)
  (require 'init-ui)
  (require 'init-modeline)
  (require 'init-completion)
  ;; TODO: wait for tty-child-frame stable
  (when (display-graphic-p)
    (require 'init-lsp-bridge))
  (require 'init-nav)
  (require 'init-evil)
  (require 'init-search)
  (require 'init-prog)
  ;;(require 'init-emigo)

  (when (file-exists-p custom-file)
    (load custom-file))

  (run-with-idle-timer
   1 nil #'(lambda ()
             ;; (require 'init-popweb)
             ;; (when (and (eq system-type 'gnu/linux) (display-graphic-p))
             ;;    (require 'init-eaf))
             ;; (require 'init-llm)
             (require 'init-dired)
             ;; (require 'init-grease)     ;; oil.nvim like file manager, which is the replacement of dired
             (require 'init-latex)
             (require 'init-org)
             (require 'init-notes)
             (require 'init-docs)
             (require 'init-tools)
             (require 'init-rime)
             (require 'init-vc)
             ))
  )
