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
  ;; (blink-search-enable-posframe t)
  ;; :config
  ;; (setq blink-search-browser-function 'browse-url-default-macosx-browser)
  :bind (:map global-map
	      ([remap isearch-forward] . blink-search))
  )

;; (use-package pdf-tools
;;   :init
;;   (setq-default pdf-view-display-size 'fit-page)
;;   :config
;;   (pdf-tools-install-noverify))


(provide 'init-tools)
