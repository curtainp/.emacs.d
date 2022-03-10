;;; init-evil.el -*- lexical-binding: t -*-


(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (evil-mode)
  (progn
    (defun ada/fix-evil-company ()
      (when (memq 'company-emulation-alist emulation-mode-map-alists)
        (company-ensure-emulation-alist)))
    (defun ada/evil-insert-hook ()
      (global-company-mode 1)
      ;; (yas-global-mode 1)
      ))

  :hook ((evil-local-mode . ada/fix-evil-company)
         (evil-insert-state-entry . ada/evil-insert-hook))

  :config
  (progn
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)
    (setq-default evil-ex-search-persistent-highlight nil)
    
    (evil-define-key 'normal dired-mode-map
      ;; "<RET>" 'dired-find-alternate-file
      "E" 'dired-toggle-read-only
      "C" 'dired-do-copy
      "`" 'dired-open-term
      "gr" 'revert-buffer
      "z" 'dired-get-size
      "c" 'dired-copy-file-here
      "f" 'consult-buffer
      ")" 'dired-omit-mode
      "<" 'beginning-of-buffer
      ">" 'end-of-buffer)
    (define-key evil-visual-state-map "p" 'evil-paste-after)

    (dolist (m '(minibuffer-inactive-mode
                 makey-key-mode
                 prodigy-mode
                 ag-mode
                 flycheck-error-list-mode
                 git-rebase-mode))
      (add-to-list 'evil-emacs-state-modes m))

    (setq evil-move-cursor-back nil)
    (define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state)
    (add-hook 'xref--xref-buffer-mode-hook
              (lambda ()
                (evil-add-hjkl-bindings xref--xref-buffer-mode-map 'normal
                  (kbd "RET") 'xref-goto-xref
                  (kbd "q") 'quit-window)))
    (add-hook 'occur-edit-mode-hook
              (lambda ()
                (evil-add-hjkl-bindings occur-edit-mode-map 'normal
                  (kbd "/") 'evil-search-forward
                  (kbd "n") 'evil-search-next
                  (kbd "N") 'evil-search-previous
                  (kbd "C-d") 'evil-scroll-down
                  (kbd "C-u") 'evil-scroll-up)))
    ))



(use-package evil-leader
  :init
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>")

  (evil-leader/set-key
   "<SPC>" 'execute-extended-command)
  :config
  (progn
    (evil-leader/set-key
      "0" 'select-window-0
      "1" 'select-window-1
      "2" 'select-window-2
      "3" 'select-window-3
      "x" 'kill-this-buffer
      "n" 'curtain-toggle-line-number
      ;; opener
      "oa" 'org-agenda
      "oc" 'org-capture
      ;; magit
      "gs" 'magit-status
      "gl" 'magit-log
      ;; helper
      "hf" 'describe-function
      "hv" 'describe-variable
      "hk" 'describe-key
      "hm" 'describe-mode
      ;; project
      "pf" 'project-find-file
      "pw" 'consult-ripgrep
      "pp" 'projectile-switch-project
      "pb" 'projectile-ibuffer
      ;; lsp
      "ls" 'lsp-describe-session
      ;; file
      "ff" 'find-file
      ;; buffer
      "bb" 'consult-buffer
      )
    ))

(use-package evil-nerd-commenter
  :init
  (define-key evil-normal-state-map (kbd "<SPC>/") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "<SPC>/") 'evilnc-comment-or-uncomment-lines)
  ;; (evilnc-default-hotkeys)
  )


(provide 'init-evil)
