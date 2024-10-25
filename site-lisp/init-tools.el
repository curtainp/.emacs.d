;; -*- lexical-binding: t; -*-

(use-package anki-editor
  :defer t
  :straight (:repo "anki-editor/anki-editor")
  )

(use-package blink-search
  :straight '(blink-search :type git :host github :repo "manateelazycat/blink-search"
			   :files (:defaults "*.el" "*.py" "backend" "core" "icons")
			   :build (:not compile))
  :custom (blink-search-common-directory '(("HOME" "~/")
					   ("CONFIG" "~/.emacs.d/")
					   ("BOOK" "~/Documents/Book/")
					   ))
  :bind (:map global-map
	      ([remap isearch-forward] . blink-search))
  )

(provide 'init-tools)
