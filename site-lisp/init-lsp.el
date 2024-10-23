;; -*- lexical-binding: t -*- 

(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
			 :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
			 :build (:not compile)) ;; disable native comp, which use python multithread.
  :custom
  (lsp-bridge-enable-in-minibuffer t)
  (lsp-bride-signature-show-function 'lsp-bridge-signature-show-with-frame)
  (acm-enable-capf t)
  (acm-enable-quick-access t)
  (acm-backend-yas-match-by-trigger-keyword t)
  (acm-enable-tabnine nil)
  (acm-enable-codeium nil)
  (acm-enable-lsp-workspace-symbol t)
  (lsp-bridge-enable-inlay-hint t)
  (lsp-bridge-user-langserver-dir (concat (expand-file-name user-emacs-directory) "langserver"))
  (lsp-bridge-user-multiserver-dir (concat (expand-file-name user-emacs-directory) "multiserver"))
  :bind (:map global-map
	      ("M-." . lsp-bridge-find-def)
	      ("M-," . lsp-bridge-find-def-return)
	      ("C-c C-d" . lsp-bridge-diagnostic-list)
	      ("C-c C-a" . lsp-bridge-code-action)
	      ("C-c C-r" . lsp-bridge-find-references)
	      ("C-c C-k" . lsp-bridge-popup-documentation)
	      ("C-c C-p" . lsp-bridge-peek)
	      ("C-c C-n" . lsp-bridge-rename)
	      ("C-c C-f" . lsp-bridge-code-format))
  :init
  (global-lsp-bridge-mode)
  :config
  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("html") . "html_tailwindcss"))
  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("css") . "css_tailwindcss"))
  )

(provide 'init-lsp)
