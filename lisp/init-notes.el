;;; init-notes.el -*- lexical-binding: t -*-

;; Simple notes for Emacs with an efficient file-naming scheme
(use-package denote
  :straight t
  :commands (denote-create-note denote-insert-link denote-show-backlinks-buffer)
  :hook (dired-mode . denote-dired-mode)
  :custom
  (denote-date-prompt-use-org-read-date t) ; And `org-read-date' is an amazing bit of tech
  :bind (:map global-map
              ("C-c n p" . denote-sequence-new-parent)
              ("C-c n c" . denote-sequence-new-child)
              ("C-c n s" . denote-sequence-new-sibling)
              ("C-c n d" . denote-sequence-dired)
              ("C-c n l" . denote-sequence-link)
              ;; :map org-mode-map
              ;; ("C-c n d l" . denote-org-extra-dblock-insert-links)
              ;; ("C-c n d b" . denote-org-extra-dblock-insert-backlinks)
              :map dired-mode-map
              ("C-c C-d C-i" . denote-dired-link-marked-notes)
              ("C-c C-d C-r" . denote-dired-rename-marked-files)
              ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
              ("C-c C-d C-f" . denote-dired-rename-marked-files-using-front-matter))
              
  :config
  (setq denote-directory curtain-org-directory
        denote-known-keywords '("emacs" "work" "blog" "journal")
        denote-infer-keywords t
        denote-sort-keywords t
        )
  (denote-rename-buffer-mode 1))

(use-package denote-sequence
  :straight t)

;; View and filter Denote files in a tabulated list
(use-package denote-menu
  :straight t)

(use-package consult-notes
  :straight (:type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :bind (:map global-map
              ("C-c n f" . consult-notes))
  :config
  ;; (setq consult-notes-file-dir-sources '(("Security" ?s "~/workspace/docs/org")))   ;; use denote-mode instead
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  (setq consult-notes-denote-files-function (lambda () (denote-directory-files nil t t))))

(use-package org-super-links
  :straight (:type git :host github :repo "toshism/org-super-links" :branch "develop")
  :bind (("C-c s s" . org-super-links-link)
         ("C-c s l" . org-super-links-store-link)
         ("C-c s C-l" . org-super-links-insert-link)
         ("C-c s d" . org-super-links-quick-insert-drawer-link)
         ("C-c s i" . org-super-links-quick-insert-inline-link)
         ("C-c s C-d" . org-super-links-delete-link))
  :config
  (setq org-super-links-related-into-drawer t
  	org-super-links-link-prefix 'org-super-links-link-prefix-timestamp))


(use-package org-download
  :straight t
  :after org
  :bind (:map org-mode-map
              ("C-c d c" . org-download-clipboard))
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  (when (eq system-type 'darwin)
    (setq org-download-screenshot-method "pngpaste %s"))
  (setq-default org-download-heading-lvl nil
                org-download-image-dir "."))

(use-package ox-hugo
  :straight t
  :after ox
  :config
  (add-to-list 'org-hugo-special-block-type-properties '("raw" . (:raw t))))

(use-package ox-zola
  :disabled
  :straight (:host github :repo "gicrisf/ox-zola"
                   :files (:defaults "*.el" "backend" "stylesheets"))
  :demand t)

(use-package org-transclusion
  :straight (:host github
                   :repo "nobiot/org-transclusion")
  :after org)


(provide 'init-notes)
