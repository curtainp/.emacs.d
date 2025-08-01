;;; -*- lexical-binding: t -*-


;; An Emacs major mode to read and browse RFC documents
(use-package rfc-mode
  :straight t
  :custom
  (rfc-mode-directory (concat user-emacs-directory "rfc"))
  :init
  ;; Use a window wide enough but not too wide
  (add-to-list
   'display-buffer-alist
   `((derived-mode . rfc-mode)
     (display-buffer-in-side-window)
     (slot . 0)
     (side . right)
     (dedicated . t) ;; Close when finished
     (window-width . 76))))

(use-package ascii
  :straight t
  :commands (ascii-on ascii-off)
  :preface
  (defun ascii-toggle()
    (interactive)
    (if ascii-display
        (ascii-off)
      (ascii-on))))


(use-package helpful
  :straight t
  :defer 3
  :bind (("<remap> <describe-variable>" . helpful-variable)
         ("<remap> <describe-symbol>" . helpful-symbol)
         ("<remap> <describe-function>" . helpful-callable)
         ("<remap> <describe-command>" . helpful-command)
         ("<remap> <describe-key>" . helpful-key)
         ("C-h h" . helpful-at-point)))

(provide 'init-docs)
