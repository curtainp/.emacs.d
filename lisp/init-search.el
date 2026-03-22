;;; -*- lexical-binding: t -*-

(setq search-whitespace-regexp ".*?" ; one `setq' here to make it obvious they are a bundle
    isearch-lax-whitespace t
    isearch-regexp-lax-whitespace nil)

(setq search-highlight t)
(setq isearch-lazy-highlight t)
(setq lazy-highlight-initial-delay 0)
(setq lazy-highlight-no-delay-length 4)


(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)

(setq list-matching-lines-jump-to-current-line nil) ; do not jump to current line in `*occur*' buffers
(add-hook 'occur-mode-hook #'hl-line-mode)

(use-package isearch
  :straight nil
  :bind
  ( :map minibuffer-local-isearch-map
    ("M-/" . isearch-complete-edit)
    :map occur-mode-map
    ("t" . toggle-truncate-lines)
    :map isearch-mode-map
    ("C-g" . isearch-cancel) ; instead of `isearch-abort'
    ("M-/" . isearch-complete)))

(provide 'init-search)
