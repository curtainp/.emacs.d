;;; -*- no-byte-compile: t; lexical-binding: t;  -*-

(defun display-startup-time ()
  "Display the startup time and number of garbage collections."
  (message "Emacs init loaded in %.2f seconds (Full emacs-startup: %.2fs) with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           (time-to-seconds (time-since before-init-time))
           gcs-done))
(add-hook 'emacs-startup-hook #'display-startup-time 100)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024))) 105)
                                        ; make gc-restore at the end of `emacs-startup-hook'

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; custom variable
(setq custom-theme-directory
      (expand-file-name "themes/" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-jit-compilation t))
;; Disable compress and uncompress output messages
(setq jka-compr-verbose nil)
(setq byte-compile-warnings nil
      byte-compile-verbose nil)

(set-language-environment "UTF-8")

(setq read-process-output-max (* 4 1024 1024))

(setq process-adaptive-read-buffering nil)
;; don't ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; ignore warnings about "existing variables being aliased"
(setq warning-suppress-types '((defvaralias) (lexical-binding)))
(setq warning-minimum-level :error)

(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; disable warnings from the legacy advice API
(setq ad-redefinition-action 'accept)

(setq inhibit-compacting-font-caches t)

(when (not noninteractive)
  (setq frame-resize-pixelwise t)
  (setq frame-inhibit-implied-resize 'force)
  (setq auto-mode-case-fold nil)
  ;; startup screen
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message t
        inhibit-startup-message t
        inhibit-startup-buffer-menu t
        inhibit-x-resources t
        inhibit-default-init t
        initial-scratch-message nil
        initial-major-mode 'fundamental-mode)
  (advice-add #'display-startup-echo-area-message :override #'ignore)
  (advice-add #'display-startup-screen :override #'ignore)
  ;; Disable [bidirectional text] scanning for a modest performance
  ;; Will improve long line display performance
  (setq bidi-inhibit-bpa t)
  (setq-default bidi-paragraph-direction 'left-to-right
                bidi-display-reordering 'left-to-right)
  (unless (eq system-type 'darwin)
    (setq command-line-ns-option-alist nil))
  (unless (memq initial-window-system '(x pgtk))
    (setq command-line-ns-option-alist nil)))

;;; Performance: File-name-handler-alist

(defvar minimal-emacs--old-file-name-handler-alist (default-toplevel-value
                                                    'file-name-handler-alist))

(defun minimal-emacs--respect-file-handlers (fn args-left)
  "Respect file handlers.
FN is the function and ARGS-LEFT is the same argument as `command-line-1'.
Emacs processes command-line files very early in startup. These files may
include special paths like TRAMP paths, so restore `file-name-handler-alist' for
this stage of initialization."
  (let ((file-name-handler-alist (if args-left
                                     minimal-emacs--old-file-name-handler-alist
                                   file-name-handler-alist)))
    (funcall fn args-left)))

(defun minimal-emacs--restore-file-name-handler-alist ()
  "Restore `file-name-handler-alist'."
  (set-default-toplevel-value
   'file-name-handler-alist
   ;; Merge instead of overwrite to preserve any changes made since startup.
   (delete-dups (append file-name-handler-alist
                        minimal-emacs--old-file-name-handler-alist))))

(progn
  ;; Determine the state of bundled libraries using calc-loaddefs.el. If
  ;; compressed, retain the gzip handler in `file-name-handler-alist`. If
  ;; compiled or neither, omit the gzip handler during startup for improved
  ;; startup and package load time.
  (set-default-toplevel-value
   'file-name-handler-alist
   (if (locate-file-internal "calc-loaddefs.el" load-path)
       nil
     (list (rassq 'jka-compr-handler
                  minimal-emacs--old-file-name-handler-alist))))

  ;; Ensure the new value persists through any current let-binding.
  (put 'file-name-handler-alist 'initial-value
       minimal-emacs--old-file-name-handler-alist)

  ;; Emacs processes command-line files very early in startup. These files may
  ;; include special paths TRAMP. Restore `file-name-handler-alist'.
  (advice-add 'command-line-1 :around #'minimal-emacs--respect-file-handlers)

  (add-hook 'emacs-startup-hook #'minimal-emacs--restore-file-name-handler-alist
            101))

;;; Performance: Inhibit redisplay

(defun minimal-emacs--reset-inhibit-redisplay ()
  "Reset inhibit redisplay."
  (setq-default inhibit-redisplay nil)
  (remove-hook 'post-command-hook #'minimal-emacs--reset-inhibit-redisplay))

(when (not noninteractive)
  ;; Suppress redisplay and redraw during startup to avoid delays and
  ;; prevent flashing an unstyled Emacs frame.
  (setq-default inhibit-redisplay t)
  (add-hook 'post-command-hook #'minimal-emacs--reset-inhibit-redisplay -100))

;;; Performance: Inhibit message

(defun minimal-emacs--reset-inhibit-message ()
  "Reset inhibit message."
  (setq-default inhibit-message nil)
  (remove-hook 'post-command-hook #'minimal-emacs--reset-inhibit-message))

(when (not noninteractive)
  (setq-default inhibit-message t)
  (add-hook 'post-command-hook #'minimal-emacs--reset-inhibit-message -100))

;;; Performance: Disable mode-line during startup
(setq-default mode-line-format nil)

;;; Restore values

(defun minimal-emacs--startup-load-user-init-file (fn &rest args)
  "Advice to reset `mode-line-format'. FN and ARGS are the function and args."
  (unwind-protect
      ;; Start up as normal
      (apply fn args)
    ;; If we don't undo inhibit-{message, redisplay} and there's an error, we'll
    ;; see nothing but a blank Emacs frame.
    (setq-default inhibit-message nil)
    (setq-default inhibit-redisplay nil)))

(advice-add 'startup--load-user-init-file :around
            #'minimal-emacs--startup-load-user-init-file)

(setq use-file-dialog nil
      use-dialog-box nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(unless (memq window-system '(mac ns))
  (setq menu-bar-mode nil))

(defun minimal-emacs--setup-toolbar (&rest _)
  "Setup the toolbar."
  (when (fboundp 'tool-bar-setup)
    (advice-remove 'tool-bar-setup #'ignore)
    (when (bound-and-true-p tool-bar-mode)
      (funcall 'tool-bar-setup))))

(unless noninteractive
  (when (fboundp 'tool-bar-setup)
    ;; Temporarily override the tool-bar-setup function to prevent it from
    ;; running during the initial stages of startup
    (advice-add 'tool-bar-setup :override #'ignore)

    (advice-add 'startup--load-user-init-file :after
                #'minimal-emacs--setup-toolbar)))

(push '(tool-bar-lines . 0) default-frame-alist)
(setq tool-bar-mode nil)
(setq default-frame-scroll-bars 'right)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(push '(undecorated-round . t) default-frame-alist)

;; This results in a more compact output that emphasizes performance
(setq package-enable-at-startup nil)
;; from Emacs31, make byte-compile lisp file under this directory and load autoloads.
;; (setq user-lisp-directory (locate-user-emacs-file "modules/"))
