;;; -*- lexical-binding: t -*-

;;(eval-when-compile
;;  (require 'init-custom)
;;  (require 'init-funcs))
;;
;;(set-package-archives curtain-package-archives nil nil t)

;; (setq url-proxy-services
;;       `(("http" . ,curtain-proxy)
;;         ("https" . ,curtain-proxy)))

(defvar straight-check-for-modifications)

(setq straight-check-for-modifications '(check-on-save find-when-checking)                   ; skip modification at startup, checking on demand
      comp-deferred-compilation-deny-list ()                 ; config native comp
      warning-suppress-log-types '((comp))                   ; Don't display comp warnings
      straight-repository-branch "develop"
      straight-disable-native-compile (not (and (fboundp 'native-comp-available-p)
                                                (native-comp-available-p))))

;; Installation
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

;; (require 'straight)

;; HACK+PERF: Reduce installation time and disk usage using "--filter=tree:0",
;; this cuts the size of the "repos" directory by more than half (from 807M to
;; 362M) while keeping it possible to download older commits on-demand (unlike
;; "--depth=N"). The parameter is injected in `straight--process-run' which is
;; called from `straight-vc-git--clone-internal'
(advice-add
 'straight--process-run :around
 (lambda (fn &rest a)
   (apply fn (if (equal (list (car a) (cadr a)) '("git" "clone")) `(,(car a) ,(cadr a) "--filter=tree:0" ,@(cddr a)) a))))

(cl-callf append straight-built-in-pseudo-packages
  '(org ; Otherwise, `straight' will try to install it as a dependency
    treesit ; Some packages like `ts-movement' depends on it
    ))

(setq use-package-always-demand (daemonp)
      use-package-always-defer (not (daemonp))
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

;; HACK+FIX: We need to install a new version, otherwise, `magit' and `forge'
;; can cause problems. This needs to be done early before the builtin
;; `transient' gets loaded.
(use-package transient
  :straight t
  :config
  (setq transient-show-popup 0.5))

(provide 'init-straight)
