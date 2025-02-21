;;; -*- lexical-binding: t -*-

(use-package treesit-auto
  :straight (:host github :repo "renzmann/treesit-auto")
  :when (featurep 'tree-sitter)
  :hook (prog-mode . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; Add extra grammars
  ;; BUG+FIX: Remove the Markdown grammar to install it correctly (renzmann/treesit-auto#102)
  (let* ((extra-recipes
          (list (make-treesit-auto-recipe
                 :lang 'xml
                 :ts-mode 'xml-ts-mode
                 :remap '(nxml-mode xml-mode)
                 :url "https://github.com/tree-sitter-grammars/tree-sitter-xml"
                 :source-dir "xml/src"
                 :ext "\\.xml\\'")
                (make-treesit-auto-recipe
                 :lang 'markdown
                 :ts-mode 'markdown-ts-mode
                 :remap '(poly-markdown-mode markdown-mode)
                 :requires 'markdown-inline
                 :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                 :revision "split_parser"
                 :source-dir "tree-sitter-markdown/src"
                 :ext "\\.md\\'")
                (make-treesit-auto-recipe
                 :lang 'markdown-inline
                 :ts-mode 'markdown-inline-mode ; Fake mode to make `treesit-auto' happy
                 :remap 'markdown-inline-ts-mode
                 :requires 'markdown
                 :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                 :revision "split_parser"
                 :source-dir "tree-sitter-markdown-inline/src")
                (make-treesit-auto-recipe
                 :lang 'llvm
                 :ts-mode 'llvm-ts-mode
                 :remap 'llvm-mode
                 :url "https://github.com/benwilliamgraham/tree-sitter-llvm"
                 :ext "\\.ll\\'")
                (make-treesit-auto-recipe
                 :lang 'elisp
                 :ts-mode 'emacs-lisp-ts-mode
                 :remap 'emacs-lisp-mode
                 :url "https://github.com/Wilfred/tree-sitter-elisp"
                 :ext "\\.eld?\\'"))))
    ;; First, delete the duplicate recipes already present in the list, if any
    (cl-callf2 cl-delete-if
        (lambda (lang) (memq (treesit-auto-recipe-lang lang) (mapcar #'treesit-auto-recipe-lang extra-recipes)))
        treesit-auto-recipe-list)
    ;; Then, add the extra recipes to the list
    (cl-callf append treesit-auto-recipe-list extra-recipes)
    (setq treesit-auto-langs (mapcar #'treesit-auto-recipe-lang treesit-auto-recipe-list)))

  ;; Ensure that installed tree-sitter languages have their corresponding `x-ts-mode' added to `auto-mode-alist'
  (treesit-auto-add-to-auto-mode-alist 'all)

  (defvar +treesit-auto-create-parser-modes-deny '(org-mode))

  ;; Create `treesit' parsers when they are available even in non-treesit modes.
  ;; This is useful for packages like `virtual-format', `treesit-fold', `expreg'
  ;; and `ts-movement'.
  (defun +treesit-auto-create-parser-in-buffer (&optional buffer)
    "Create `treesit' in BUFF-NAME, even if the mode isn't a ts-mode."
    (interactive (list (when current-prefix-arg (get-buffer (read-buffer "Create treesit parser in buffer: ")))))
    (let ((buffer (or buffer (current-buffer)))
          (interact-p (called-interactively-p 'interactive)))
      (if (treesit-available-p)
          (when (or (not (derived-mode-p +treesit-auto-create-parser-modes-deny))
                    (and interact-p (y-or-n-p "Creating parsers for `%S' is blacklisted in `+treesit-auto-create-parser-modes-deny', continue?")))
            (with-current-buffer buffer
              (if-let* ((lang-recipe (cl-find-if (lambda (recipe) (eq major-mode (treesit-auto-recipe-remap recipe)))
                                                 treesit-auto-recipe-list))
                        (lang (treesit-auto-recipe-lang lang-recipe))
                        (lang (and (treesit-language-available-p lang) lang)))
                  (when (or (not (treesit-parser-list buffer lang))
                            (and interact-p (y-or-n-p (format "The %S buffer already have a %S language parser, continue?" buffer lang))))
                    (treesit-parser-create lang)
                    (when interact-p (message "Created a %S language parser in %S" lang buffer)))
                (when interact-p (user-error "No installed tree-sitter grammar for mode `%s'" major-mode)))))
        (when interact-p (user-error "Tree-sitter isn't available in this Emacs build")))))

  (add-hook 'after-change-major-mode-hook '+treesit-auto-create-parser-in-buffer))

;; Highlight TODO keywords
(use-package hl-todo
  :straight (:host github :repo "tarsius/hl-todo")
  :hook (prog-mode . hl-todo-mode)
  :config
  (cl-callf append hl-todo-keyword-faces
    '(("BUG"   . "#ee5555")
      ("FIX"   . "#0fa050")
      ("PROJ"  . "#447f44")
      ("IDEA"  . "#0fa050")
      ("INFO"  . "#0e9030")
      ("TWEAK" . "#fe9030")
      ("PERF"  . "#e09030"))))

(use-package rust-mode
  :straight t
  :mode ("\\.rs\\'" . rust-ts-mode)
  :init
  (setq rust-mode-treesitter-derive t
        rust-format-goto-problem nil))

(provide 'init-prog)
