;;; -*- lexical-binding: t -*-

(use-package latex
  :straight auctex
  :hook ((LaTeX-mode . prettify-symbols-mode))
  :bind (:map latex-mode-map
              ("C-S-e" . latex-math-from-calc))
  :config
  ;; Format math as a Latex string with `calc'
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (cond ((region-active-p)
           (let* ((beg (region-beginning))
                  (end (region-end))
                  (string (buffer-substring-no-properties beg end)))
             (kill-region beg end)
             (insert (calc-eval `(,string calc-language latex
                                  calc-prefer-frac t
                                  calc-angle-mode rad)))))
          (t (let ((l (thing-at-point 'line)))
               (end-of-line 1) (kill-line 0)
               (insert (calc-eval `(,l
                                    calc-language latex
                                    calc-prefer-frac t
                                    calc-angle-mode rad))))))))

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
