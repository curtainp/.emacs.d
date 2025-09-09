;; init-lsp-bridge.el -*- lexical-binding: t -*-

(use-package yasnippet
  :straight t
  :hook ((text-mode prog-mode conf-mode markdown-mode) . yas-minor-mode)
  :custom
  (yas-triggers-in-field t))

(use-package lsp-bridge
  :straight '(:type git :host github :repo "curtainp/lsp-bridge"
                    :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                    :build (:not compile))
  :custom-face
  (lsp-bridge-inlay-hint-face ((t (:foreground "#5B6268"))))
  :config
  ;; (lsp-bridge-enable-in-minibuffer t)
  (setq lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
  (setq lsp-bridge-python-multi-lsp-server 'basedpyright_ruff)
  (setq lsp-bridge-enable-hover-diagnostic t)
  (setq acm-enable-capf t)
  (setq acm-enable-icon t)
  (setq acm-enable-tabnine nil)
  (setq acm-enable-codeium nil)
  (setq acm-enable-quick-access nil)
  ;; (acm-backend-yas-match-by-trigger-keyword t)
  (setq acm-enable-lsp-workspace-symbol t)
  (setq lsp-bridge-semantic-tokens t)
  (setq-default lsp-bridge-semantic-tokens-ignore-modifier-limit-types ["variable"])
  ;; (lsp-bridge-enable-inlay-hint t)
  ;; (lsp-bridge-get-language-id 'get-tailwindcss-language-id-in-react)
  (setq lsp-bridge-user-langserver-dir (concat (expand-file-name user-emacs-directory) "langserver"))
  (setq lsp-bridge-user-multiserver-dir (concat (expand-file-name user-emacs-directory) "multiserver"))
  (setq lsp-bridge-log-level 'error)
  (setq lsp-bridge-multi-lang-server-extension-list
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
