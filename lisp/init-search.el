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
        ("C-." . isearch-forward-symbol-at-point) ; easier than M-s .
        :map minibuffer-local-isearch-map
        ("M-/" . isearch-complete-edit)
        :map occur-mode-map
        ("t" . toggle-truncate-lines)
        :map isearch-mode-map
        ("C-g" . isearch-cancel) ; instead of `isearch-abort'
        ("M-/" . isearch-complete)))

(let ((ripgrep (or (executable-find "rg") (executable-find "ripgrep"))))
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
  (setq xref-show-xrefs-function #'xref-show-definitions-buffer)
  (setq xref-file-name-display 'project-relative)
  (setq xref-search-program (if ripgrep 'ripgrep 'grep))
  (setq grep-save-buffers nil)
  (setq grep-use-headings nil)
  (setq grep-program (or ripgrep (executable-find "grep")))
  (setq grep-template
        (if ripgrep
            "/usr/bin/rg -nH --null -e <R> <F>"
          "/usr/bin/grep <X> <C> -nH --null -e <R> <F>")))

;; `grep-edit-mode' built-in Emacs 31


(provide 'init-search)
