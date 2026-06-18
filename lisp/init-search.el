;;; -*- lexical-binding: t -*-

(setq search-whitespace-regexp ".*?"
      isearch-lax-whitespace t
      isearch-regexp-lax-whitespace nil)

(setq search-highlight t)
(setq isearch-lazy-highlight t)
(setq lazy-highlight-initial-delay 0.5)
(setq lazy-highlight-no-delay-length 4)


(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)

(setq isearch-wrap-pause t)
(setq isearch-repeat-on-direction-change t)

(setq list-matching-lines-jump-to-current-line nil) ; do not jump to current line in `*occur*' buffers
(add-hook 'occur-mode-hook #'hl-line-mode)

(use-package isearch
  :straight nil
  :bind
  (:map global-map
        ;; ("C-." . isearch-forward-symbol-at-point) ; easier than M-s .
        :map minibuffer-local-isearch-map
        ("M-/" . isearch-complete-edit)
        :map occur-mode-map
        ("t" . toggle-truncate-lines)
        :map isearch-mode-map
        ("C-g" . isearch-cancel) ; instead of `isearch-abort'
        ("M-/" . isearch-complete)))

(use-package dumb-jump
  :straight t
  :custom
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-rg-search-args "--pcre2 --follow") ;; follow symbolic links
  (dumb-jump-rust-search-dependencies t)
  (xref-search-program 'ripgrep)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; `grep-edit-mode' built-in Emacs 31

(use-package visual-replace
  :straight t
  :commands (visual-replace visual-replace-from-isearch)
  :bind (("C-c r" .  visual-replace)
         ([remap query-replace] . visual-replace)
         ([remap replace-string] . visual-replace)
         ([remap isearch-query-replace] . visual-replace-from-isearch)
         ([remap isearch-query-replace-regexp] . visual-replace-from-isearch)
         :map isearch-mode-map
         ("C-c r" . visual-replace-from-isearch))
  :config
  (setq visual-replace-default-to-full-scope t))

(provide 'init-search)
