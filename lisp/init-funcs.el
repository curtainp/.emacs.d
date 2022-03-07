;;; init-funcs.el -*- lexical-binding: t -*-

(require 'init-custom)

(defun curtain-set-variable (variable value &optional no-save)
  "Set the VARIABLE to VALUE, and return VALUE.
Save to `custom-file' if NO-SAVE is nil."
  (customize-set-variable variable value)
  (when (and (not no-save)
             (file-writable-p custom-file))
    (with-temp-buffer
      (insert-file-contents custom-file)
      (goto-char (point-min))
      (while (re-search-forward
              (format "^[\t ]*[;]*[\t ]*(setq %s .*)" variable)
              nil t)
        (replace-match (format "(setq %s '%s)" variable value) nil nil))
      (write-region nil nil custom-file)
      (message "Saved %s (%s) to %s" variable value custom-file))))

(defun curtain-toggle-line-number ()
  (interactive)
  (if global-display-line-numbers-mode
      (menu-bar--display-line-numbers-mode-relative)
    (global-display-line-numbers-mode)))

;; Pakcage repository (ELPA)
(defun set-package-archives (archives &optional refresh async no-save)
  "Set the package archives (ELPA).
REFRESH is non-nil, will refresh archive contents.
ASYNC specifies whether to perform the downloads in the background.
Save to `custom-file' if NO-SAVE is nil."
  (interactive
   (list
    (intern (completing-read "Select package archives: "
                             (mapcar #'car curtain-package-archives-alist)))))
  ;; Set option
  (curtain-set-variable 'curtain-package-archives archives no-save)

  ;; Refresh if need
  (and refresh (package-refresh-contents async))

  ;; (message "Set package archives to `%s'" archives)
  )
(defalias 'curtain-set-package-archives #'set-package-archives)


(provide 'init-funcs)
