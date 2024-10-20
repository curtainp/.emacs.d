;; -*- lexical-binding: t -*- 

(use-package eglot
     :hook ((prog-mode . (lambda ()
                           (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                             (eglot-ensure))))
            ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
     :init
     (setq read-process-output-max (* 1024 1024)) ; 1MB
     (setq eglot-autoshutdown t
           eglot-events-buffer-size 0
           eglot-send-changes-idle-time 0.5)
     :config
     (use-package consult-eglot
       :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)))

     ;; Emacs LSP booster
     (when (executable-find "emacs-lsp-booster")
       (unless (package-installed-p 'eglot-booster)
         (and (fboundp #'package-vc-install)
              (package-vc-install "https://github.com/jdtsmith/eglot-booster")))
       (use-package eglot-booster
         :autoload eglot-booster-mode
         :init (eglot-booster-mode 1))))

(provide 'init-lsp)
