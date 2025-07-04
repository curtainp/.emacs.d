;; init-lsp-bridge.el -*- lexical-binding: t -*-

(use-package yasnippet
  :straight t
  :hook ((text-mode prog-mode conf-mode markdown-mode) . yas-minor-mode)
  :custom
  (yas-triggers-in-field t))

(use-package lsp-bridge
  :straight '(:type git :host github :repo "manateelazycat/lsp-bridge"
                    :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                    :build (:not compile))
  :custom-face
  (lsp-bridge-inlay-hint-face ((t (:foreground "#5B6268"))))
  :custom
  ;; (lsp-bridge-enable-in-minibuffer t)
  (lsp-bride-signature-show-function 'lsp-bridge-signature-show-with-frame)
  (lsp-bridge-python-multi-lsp-server 'basedpyright_ruff)
  (lsp-bridge-enable-hover-diagnostic t)
  (acm-enable-capf t)
  (acm-enable-icon t)
  (acm-enable-quick-access nil)
  ;; (acm-backend-yas-match-by-trigger-keyword t)
  (acm-enable-tabnine nil)
  (acm-enable-codeium nil)
  (acm-enable-lsp-workspace-symbol t)
  (lsp-bridge-enable-inlay-hint t)
  ;; (lsp-bridge-get-language-id 'get-tailwindcss-language-id-in-react)
  (lsp-bridge-user-langserver-dir (concat (expand-file-name user-emacs-directory) "langserver"))
  (lsp-bridge-user-multiserver-dir (concat (expand-file-name user-emacs-directory) "multiserver"))
  (lsp-bridge-log-level 'error)
  (lsp-bridge-multi-lang-server-extension-list
	'(
	  (("ts")   . "typescript_eslint")
	  (("tsx")  . "typescriptreact_tailwindcss")
	  (("jsx")  . "javascriptreact_tailwindcss")
	  (("html") . "html_tailwindcss")
	  (("css")  . "css_tailwindcss")))
  :init
  (when (eq system-type 'darwin)
    (setq lsp-bridge-python-command "/opt/homebrew/bin/python3"))
  (global-lsp-bridge-mode))

(provide 'init-lsp-bridge)
