;;; init-completion.el -*- lexical-binding: t -*-

(use-package vertico
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-resize nil
        vertico-count 15
        vertico-cycle t)
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (define-key vertico-map (kbd "C-n") 'vertico-next)
  (define-key vertico-map (kbd "C-p") 'vertico-previous)
  (define-key vertico-map [backspace] #'vertico-directory-delete-char)
  ;;(define-key vertico-map (kbd "s-SPC") #'+vertico/embark-preview)
  )

(use-package marginalia
  :hook (after-init . marginalia-mode)
  :init
  :config
  )

(use-package orderless
  :demand t
  ;;       orderless-component-separator "[ &]")
  ;; ...otherwise find-file gets different highlighting than other commands
  ;; (set-face-attribute 'completions-first-difference nil :inherit nil)
  :config
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))

  ;; Recognizes the following patterns:
  ;; * ~flex flex~
  ;; * =literal literal=
  ;; * %char-fold char-fold%
  ;; * `initialism initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-dispatch (pattern index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; File extensions
     ((and
       ;; Completing filename or eshell
       (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
       ;; File extension
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
                                     (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
        completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
                                        ;; enable initialism by default for symbols
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers '(+orderless-dispatch))
  )

(use-package consult
  :defer t
  :init
  (global-set-key (kbd "s-v") 'consult-yank-pop)
  (global-set-key (kbd "C-s") 'consult-line)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (advice-add #'multi-occur :override #'consult-multi-occur)
  ;;(advice-add #'consult-line :override #'consult-line)
              
  :config
  (setq ;; consult-project-root-function #'doom-project-root
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)


  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   :preview-key (kbd "s-.")
  )

  (consult-customize
   consult-theme
   :preview-key (list (kbd "s-.") :debounce 0.5 'any))
  )

;; enable in evil insert mode
(use-package company
  ;; :ensure nil
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
        company-show-numbers t
        company-cmake-executable-arguments '("--help-command-list"
                                             "--help-module-list"
                                             "--help-property-list"
                                             "--help-variable-list")
        company-backends '((company-capf company-dabbrev-code company-dabbrev
                                         company-keywords)
                           (company-files)))

  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("M-i" . company-complete-selection)))

(use-package eglot
  ;; :ensure nil
  :commands (eglot-ensure my/rust-expand-macro)
  :config
  (progn
    (setq eldoc-echo-area-use-multiline-p 3
          eldoc-echo-area-display-truncation-message nil)
    (set-face-attribute 'eglot-highlight-symbol-face nil
                        :background "#b3d7ff")

    (defun my/rust-expand-macro ()
      "Expand macro at point, same as `lsp-rust-analyzer-expand-macro'.
        https://rust-analyzer.github.io/manual.html#expand-macro-recursively"
      (interactive)
      (jsonrpc-async-request
       (eglot--current-server-or-lose)
       :rust-analyzer/expandMacro (eglot--TextDocumentPositionParams)
       :error-fn (lambda (msg) (error "Macro expand failed, msg:%s." msg))
       :success-fn
       (lambda (expanded-macro)
	     (cl-destructuring-bind (name format expansion result) expanded-macro
	       (let* ((pr (eglot--current-project))
			      (buf (get-buffer-create (format "*rust macro expansion %s*" (cdr pr)))))
		     (with-current-buffer buf
		       (let ((inhibit-read-only t))
			     (erase-buffer)
			     (insert result)
			     (rust-mode)))
		     (switch-to-buffer-other-window buf))))))
    ))


(provide 'init-completion)
