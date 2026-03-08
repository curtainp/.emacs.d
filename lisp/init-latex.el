;;; -*- lexical-binding: t -*-

(use-package latex
  :straight auctex
  :hook ((LaTeX-mode . prettify-symbols-mode)))

(use-package preview
  :straight nil
  :after latex
  :hook ((LaTeX-mode . preview-larger-previews))
  :config
  (defun preview-larger-previews ()
    (setq preview-scale-function
          (lambda () (* 1.25
                        (funcall (preview-scale-from-face)))))))

(use-package cdlatex
  :straight t
  :hook (LaTeX-mode . turn-on-org-cdlatex)
  :bind (:map cdlatex-mode-map
              ("<tab>" . cdlatex-tab)))


(provide 'init-latex)
