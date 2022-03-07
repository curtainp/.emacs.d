;;; init-evil.el -*- lexical-binding: t -*-


(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (evil-mode)

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

    (dolist (m '(wdired-mode
                 occur-edit-mode
                 xref--xref-buffer-mode
                 ))
      (add-to-list 'evil-normal-state-modes m))

    (setq evil-move-cursor-back nil)
    (define-key evil-emacs-state-map (kbd "C-w") 'evil-delete-backward-word)
    (evil-define-key 'emacs term-raw-map (kbd "C-w") 'evil-delete-backward-word)
    (setq evil-normal-state-tag   (propertize "[N]" 'face '((:background "DarkGoldenrod2" :foreground "black")))
          evil-emacs-state-tag    (propertize "[E]" 'face '((:background "SkyBlue2" :foreground "black")))
          evil-insert-state-tag   (propertize "[I]" 'face '((:background "chartreuse3") :foreground "white"))
          evil-motion-state-tag   (propertize "[M]" 'face '((:background "plum3") :foreground "white"))
          evil-visual-state-tag   (propertize "[V]" 'face '((:background "gray" :foreground "black")))
          evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))
    (define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state)
    (add-hook 'xref--xref-buffer-mode-hook
              (lambda ()
                (evil-add-hjkl-bindings xref--xref-buffer-mode-map 'normal
                  (kbd "RET") 'xref-goto-xref
                  (kbd "q") 'quit-window)))
    (add-hook 'occur-mode-hook
              (lambda ()
                (evil-add-hjkl-bindings occur-mode-map 'emacs
                  (kbd "/") 'evil-search-forward
                  (kbd "n") 'evil-search-next
                  (kbd "N") 'evil-search-previous
                  (kbd "C-d") 'evil-scroll-down
                  (kbd "C-u") 'evil-scroll-up)))
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
      "x" 'kill-buffer
      "n" 'curtain-toggle-line-number
      ;; opener
      "oa" 'org-agenda
      "oc" 'org-capture
      "og" 'magit-status
      ;; helper
      "hf" 'describe-function
      "hv" 'describe-variable
      "hk" 'describe-key
      "hm" 'describe-mode
      ":" 'execute-extended-command
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

(use-package magit
  :config
  (with-eval-after-load 'evil
    (evil-add-hjkl-bindings magit-status-mode-map
      'emacs
      (kbd "l") 'nil
      (kbd "h") 'nil
      (kbd "C-u") 'evil-scroll-up
      (kbd "C-d") 'evil-scroll-down
      (kbd "K") 'magit-discard
      (kbd "s-1") 'magit-jump-to-unstaged
      (kbd "s-2") 'magit-jump-to-untracked)))

(provide 'init-evil)
