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
  :init
  (global-lsp-bridge-mode)
  :config
  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("html") . "html_tailwindcss"))
  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("css") . "css_tailwindcss"))
  )

(provide 'init-lsp)
