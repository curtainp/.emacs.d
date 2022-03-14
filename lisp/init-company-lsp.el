;;;  init-company-lsp.el -*- lexical-binding: t -*-

;; enable in evil insert mode
(use-package company
  :diminish
  :commands (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-idle-delay 0
        company-begin-commands '(self-insert-command org-self-insert-command)
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        ;; company-echo-delay 0
        ;; Easy navigation to candidates with M-<n>
        company-quick-access-keys nil
        company-global-modes '(not erc-mode message-mode help-mode
                                   gud-mode eshell-mode shell-mode)
        company-backends '((company-capf company-dabbrev-code company-dabbrev
                                         company-keywords)
                           (company-files)))
  ;; yasnippet integration
  (with-no-warnings
    (with-eval-after-load 'yasnippet
      (defun company-backend-with-yas (backend)
        "Add `yasnippet' to company backend."
        (if (and (listp backend) (member 'company-yasnippet backend))
            backend
          (append (if (consp backend) backend (list backend))
                  '(:with company-yasnippet))))

      (defun my-company-enbale-yas (&rest _)
        "Enable `yasnippet' in `company'."
        (setq company-backends (mapcar #'company-backend-with-yas company-backends)))

      (defun my-lsp-fix-company-capf ()
        "Remove redundant `comapny-capf'."
        (setq company-backends
              (remove 'company-backends (remq 'company-capf company-backends))))
      (advice-add #'lsp-completion--enable :after #'my-lsp-fix-company-capf)

      (defun my-company-yasnippet-disable-inline (fn cmd &optional arg &rest _ignore)
        "Enable yasnippet but disable it inline."
        (if (eq cmd  'prefix)
            (when-let ((prefix (funcall fn 'prefix)))
              (unless (memq (char-before (- (point) (length prefix)))
                            '(?. ?< ?> ?\( ?\) ?\[ ?{ ?} ?\" ?' ?`))
                prefix))
          (progn
            (when (and (bound-and-true-p lsp-mode)
                       arg (not (get-text-property 0 'yas-annotation-patch arg)))
              (let* ((name (get-text-property 0 'yas-annotation arg))
                     (snip (format "%s (Snippet)" name))
                     (len (length arg)))
                (put-text-property 0 len 'yas-annotation snip arg)
                (put-text-property 0 len 'yas-annotation-patch t arg)))
            (funcall fn cmd arg))))
      (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline)))

  ;; better sorting and filtering
  (use-package company-prescient
    :init (company-prescient-mode 1))

  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("M-i" . company-complete-selection)
              ("<backtab>" . my-company-yasnippet))
  :init
  (defun my-company-yasnippet ()
    "Hide the current completions and show snippets."
    (interactive)
    (company-cancel)
    (call-interactively 'company-yasnippet))
  )

(use-package lsp-mode
  :diminish
  :commands (lsp-format-buffer
             lsp-organize-imports
             lsp-deferred)
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode)
                          (lsp-deferred))))
         (lsp-mode . (lambda ()
                       (evil-define-key 'normal lsp-mode-map
                                            "gd" 'lsp-find-definition
                                            "gD" 'lsp-find-declaration
                                            "K"  'lsp-describe-thing-at-point
                                            "gi" 'lsp-find-implementation
                                            "gc" 'lsp-execute-code-action
                                            "gn" 'lsp-rename
                                            "gr" 'lsp-find-references))))
  :init
  ;; https://emacs-lsp.github.io/lsp-mode/page/settings/
  (setq
   lsp-keymap-prefix ""
   lsp-keep-workspace-alive nil
   lsp-signature-auto-activate nil
   lsp-signature-render-documentation nil
   ;; lsp-ui-sideline-enable nil
   ;; lsp-ui-sideline-show-code-actions nil
   lsp-enable-indentation nil
   lsp-enable-on-type-formatting nil
   lsp-display-inline-image nil
   lsp-enable-dap-auto-configure nil
   lsp-enable-folding nil
   lsp-enable-suggest-server-download nil
   ))

(provide 'init-company-lsp)
