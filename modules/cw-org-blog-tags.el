;;; cw-org-blog-tags.el --- Complete tags from blog articles -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'org)
(require 'org-pcomplete)

(defun cw/org-pcomplete-filetags ()
  "Complete FILETAGS using a valid list of candidates.
Work around `org-pcomplete' passing a bare string as its completion
table, which signals `invalid-function' when Corfu queries it."
  (pcomplete-here
   (when org-file-tags
     (list (mapconcat #'identity org-file-tags " ")))))

;; Corfu skips the blog CAPF below for short prefixes, so this fallback
;; must also work at an empty tag or immediately after a colon.
(advice-add 'pcomplete/org-mode/file-option/filetags
            :override #'cw/org-pcomplete-filetags)

(defvar cw-emacs-blog-directory)

(defvar cw/org-blog--tag-cache (make-hash-table :test #'equal)
  "Article file names mapped to (SIGNATURE . TAGS).")

(defconst cw/org-blog--generated-files '("index.org" "tags.org" "rss.org")
  "Generated Org files that do not supply article tags.")

(defun cw/org-blog-buffer-p (&optional file)
  "Return non-nil when FILE or the current buffer is a blog article."
  (let ((file (or file buffer-file-name))
        (directory (bound-and-true-p cw-emacs-blog-directory)))
    (and file directory
         (equal (file-name-extension file) "org")
         (not (string-prefix-p ".#" (file-name-nondirectory file)))
         (not (member (file-name-nondirectory file)
                      cw/org-blog--generated-files))
         (file-in-directory-p file directory))))

(defun cw/org-blog--buffer-filetags ()
  "Read FILETAGS from the whole current buffer without running mode hooks."
  (save-excursion
    (save-restriction
      (widen)
      ;; The Org parser also works in temporary buffers without `org-mode'.
      ;; It excludes FILETAGS examples inside source and example blocks.
      (let ((major-mode 'org-mode))
        (delete-dups
         (cl-loop for value in (cdr (assoc "FILETAGS"
                                          (org-collect-keywords '("FILETAGS"))))
                  append (split-string value "[:[:space:]]+" t)))))))

(defun cw/org-blog--file-tags (file)
  "Return cached tags for FILE, preferring its live buffer contents."
  (let* ((buffer (find-buffer-visiting file))
         (attributes (unless buffer (file-attributes file)))
         (signature
          (if buffer
              (with-current-buffer buffer
                (list buffer (buffer-chars-modified-tick)))
            (list (file-attribute-modification-time attributes)
                  (file-attribute-status-change-time attributes)
                  (file-attribute-size attributes))))
         (cached (gethash file cw/org-blog--tag-cache)))
    (if (equal signature (car cached))
        (cdr cached)
      (let ((tags
             (if buffer
                 (with-current-buffer buffer
                   (cw/org-blog--buffer-filetags))
               (with-temp-buffer
                 (setq default-directory (file-name-directory file))
                 (insert-file-contents file)
                 (cw/org-blog--buffer-filetags)))))
        (puthash file (cons signature tags) cw/org-blog--tag-cache)
        tags))))

(defun cw/org-blog-tag-candidates ()
  "Return distinct FILETAGS from articles in `cw-emacs-blog-directory'.
Check for added, changed and removed files on each request, but only
parse changed articles.  Include unsaved edits and new article buffers."
  (let* ((directory (bound-and-true-p cw-emacs-blog-directory))
         (files (when (and directory (file-directory-p directory))
                  (cl-remove-if-not
                   #'cw/org-blog-buffer-p
                   (directory-files-recursively
                    (expand-file-name directory) "\\.org\\'"))))
         tags)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (cw/org-blog-buffer-p)
          (cl-pushnew (expand-file-name buffer-file-name) files :test #'equal))))
    (maphash (lambda (file _entry)
               (unless (member file files)
                 (remhash file cw/org-blog--tag-cache)))
             cw/org-blog--tag-cache)
    (dolist (file files)
      (setq tags (append (cw/org-blog--file-tags file) tags)))
    (sort (delete-dups tags) #'string-lessp)))

(defun cw/org-blog-refresh-tags ()
  "Rebuild the blog tag cache from article sources and live buffers."
  (interactive)
  (clrhash cw/org-blog--tag-cache)
  (message "Blog tag cache: %d tags" (length (cw/org-blog-tag-candidates))))

(defun cw/org-blog-filetags-completion-at-point ()
  "Complete the current FILETAGS tag using tags from blog articles."
  (when (cw/org-blog-buffer-p)
    (let* ((case-fold-search t)
           (value-start
            (save-excursion
              (beginning-of-line)
              (when (looking-at "[ \t]*#\\+FILETAGS:[ \t]*")
                (match-end 0)))))
      (when (and value-start (>= (point) value-start)
                 (eq (org-element-type (org-element-at-point)) 'keyword))
        (let ((beg (save-excursion
                     (skip-chars-backward "^: \t\n" value-start)
                     (point)))
              (end (save-excursion
                     (skip-chars-forward "^: \t\n" (line-end-position))
                     (point))))
          ;; Do not offer the unfinished tag itself as an exact match.
          (list beg end
                (delete (buffer-substring-no-properties beg end)
                        (cw/org-blog-tag-candidates))
                :exclusive t))))))

(defun cw/org-blog-setup-tag-completion ()
  "Enable article tag completion in the current Org buffer."
  (add-hook 'completion-at-point-functions
            #'cw/org-blog-filetags-completion-at-point nil t))

(add-hook 'org-mode-hook #'cw/org-blog-setup-tag-completion)

(provide 'cw-org-blog-tags)
;;; cw-org-blog-tags.el ends here
