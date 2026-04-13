;;; init-custom.el --- Shared custom options -*- lexical-binding: t; -*-

(defgroup curtain nil
  "Curtain Emacs Configuration Group."
  :group 'convenience)

(defcustom curtain-full-name user-full-name
  "Set user full name within Emacs."
  :group 'curtain
  :type 'string)

;; TODO: make this being a list of email address.
(defcustom curtain-email-address "curtainwk@gmail.com"
  "Set user email address within Emacs."
  :group 'curtain
  :type 'string)

(defcustom curtain-proxy "127.0.0.1:7890"
  "Emacs proxy services."
  :group 'curtain
  :type 'string)

(defcustom curtain-org-directory (expand-file-name "~/Documents/notes/")
  "Set org directory."
  :group 'curtain
  :type 'string)

(defcustom curtain-blog-directory (expand-file-name "~/Documents/blog/content/posts/")
  "Set org blog directory."
  :group 'curtain
  :type 'string)

(defcustom curtain-server-p nil
  "Enable `server-mode' or not."
  :group 'curtain
  :type 'boolean)

(defcustom curtain-icons-p t
  "Display icons or not. Which need install packages to support it."
  :group 'curtain
  :type 'boolean)

(defcustom curtain-lsp-client 'eglot
  "The LSP client to use.
`lsp-bridge' uses lsp-bridge with acm completion.
`eglot' uses eglot with corfu, cape, and emacs-lsp-booster."
  :group 'curtain
  :type '(choice
          (const :tag "LSP Bridge" lsp-bridge)
          (const :tag "Eglot" eglot)))

(provide 'init-custom)
;;; init-custom.el ends here
