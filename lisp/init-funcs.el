;; init-funcs.el -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(defun curtain-set-variable (variable value &optional no-save)
  "Set the VARIABLE to VALUE, and return VALUE.

  Save to option `custom-file' if NO-SAVE is nil."
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

;; Pakcage repository (ELPA)
(defun set-package-archives (archives &optional refresh async no-save)
  "Set the package ARCHIVES (ELPA).

REFRESH is non-nil, will refresh archive contents.
ASYNC specifies whether to perform the downloads in the background.
Save to option `custom-file' if NO-SAVE is nil."
  (interactive
   (list
    (intern
     (completing-read "Select package archives: "
                      (mapcar #'car curtain-package-archives-alist)))))
  ;; Set option
  (curtain-set-variable 'curtain-package-archives archives no-save)

  ;; Refresh if need
  (and refresh (package-refresh-contents async))

  (message "Set package archives to `%s'" archives))
(defalias 'curtain-set-package-archives #'set-package-archives)

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and curtain-icons-p
       (or (featurep 'nerd-icons)
           (require 'nerd-icons nil t))))

(provide 'init-funcs)
