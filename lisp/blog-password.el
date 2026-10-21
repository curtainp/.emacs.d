;;; blog-password.el --- Password protection for blog exports -*- lexical-binding: t; -*-

(require 'ox-html)
(require 'ox-publish)
(require 'json)
(require 'subr-x)

(defconst cw/blog-password--encrypt-script
  (expand-file-name "../scripts/encrypt-article.cjs"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defconst cw/blog-password--lock
  (concat "<span class=\"article-lock\" role=\"img\" aria-label=\"Password protected\""
          " title=\"Password protected\">"
          "<svg viewBox=\"0 0 24 24\" width=\"16\" height=\"16\" aria-hidden=\"true\" focusable=\"false\""
          "><use href=\"/static/article-icons.svg#lock\"/></svg></span>"))

(defun cw/blog-password--file-protected-p (file)
  "Return non-nil if FILE contains a PASSWORD keyword, including an empty one."
  (when (and (file-readable-p file) (not (file-directory-p file)))
    (org-with-file-buffer file
      (assoc "PASSWORD" (org-collect-keywords '("PASSWORD"))))))

(defun cw/blog-password--encrypt (password contents)
  "Encrypt CONTENTS using PASSWORD, stopping export if encryption fails."
  (unless (and (stringp password) (not (string-empty-p (string-trim password))))
    (error "Empty #+password: remove the keyword to publish without a password"))
  (let ((node (executable-find "node"))
        (coding-system-for-read 'utf-8-unix)
        (coding-system-for-write 'utf-8-unix))
    (unless node (error "Password-protected articles require Node.js to publish"))
    (with-temp-buffer
      (insert (json-serialize `((password . ,password) (content . ,contents))))
      (unless (eq 0 (call-process-region (point-min) (point-max) node t t nil
                                        cw/blog-password--encrypt-script))
        (error "Article encryption failed; publication stopped"))
      (buffer-string))))

(defun cw/blog-password--gate (payload)
  "Build the password form containing encrypted PAYLOAD only."
  (concat
   "<section class=\"password-gate\" aria-labelledby=\"password-heading\">\n"
   "<h2 id=\"password-heading\">Password-protected article</h2>\n"
   "<p>Enter the password to read this article.</p>\n"
   "<form class=\"password-form\">\n"
   "<label for=\"article-password\">Password</label>\n"
   "<div class=\"password-controls\">"
   "<input id=\"article-password\" type=\"password\""
   " autocomplete=\"current-password\" aria-describedby=\"password-message\" required>"
   "<button type=\"submit\" disabled>Unlock</button></div>\n"
   "<p id=\"password-message\" class=\"password-message\" role=\"status\" aria-live=\"polite\"></p>\n"
   "</form>\n<noscript>Enable JavaScript to unlock this article.</noscript>\n"
   "</section>\n<script id=\"encrypted-article\" type=\"application/json\">"
   payload "</script>\n"))

(defun cw/blog-password--template (original contents info)
  "Encrypt article CONTENTS before passing them to the ORIGINAL template with INFO."
  (let ((password (plist-get info :blog-password)))
    (if (null password)
        (funcall original contents info)
      (let* ((info (copy-sequence info))
             (subtitle (plist-get info :subtitle))
             (payload (cw/blog-password--encrypt
                       password
                       (concat (when subtitle
                                 (format "<p class=\"subtitle\">%s</p>\n"
                                         (org-export-data subtitle info)))
                               contents))))
        ;; Keep descriptions and subtitles out of public metadata.
        (setq info (plist-put info :description "Password-protected article.")
              info (plist-put info :subtitle nil)
              info (plist-put info :html-head-extra
                              (concat (plist-get info :html-head-extra)
                                      "\n<meta name=\"article-password-protected\" content=\"true\">"
                                      "\n<script src=\"/static/scripts/password.js\" defer></script>")))
        (replace-regexp-in-string
         "<h1 class=\"title\">"
         (concat "<h1 class=\"title\">" cw/blog-password--lock)
         (funcall original (cw/blog-password--gate payload) info) t t)))))

(defun cw/blog-password--sitemap-entry (original entry style project)
  "Add a lock to the title produced by ORIGINAL for ENTRY, STYLE and PROJECT."
  (let ((result (funcall original entry style project)))
    (if (and (cw/blog-password--file-protected-p
              (org-publish--expand-file-name entry project))
             (string-match (concat "\\]\\[\\(?:" (regexp-quote cw/blog-pin-macro) "\\)?") result))
        (concat (substring result 0 (match-end 0)) "{{{lock()}}}"
                (substring result (match-end 0)))
      result)))

(defun cw/blog-password--abstract (original file)
  "Withhold the abstract of protected FILE from ORIGINAL's RSS output."
  (if (cw/blog-password--file-protected-p file)
      "Password-protected article. Open the article to enter its password."
    (funcall original file)))

(defun cw/blog-password--rss-entry (original entry style project)
  "Mark a protected RSS ENTRY title returned by ORIGINAL for STYLE and PROJECT."
  (concat (when (cw/blog-password--file-protected-p
                 (org-publish--expand-file-name entry project))
            "🔒 ")
          (funcall original entry style project)))

;; Load this module after the existing `blog' backend and publishing functions.
(let ((backend (org-export-get-backend 'blog)))
  (unless backend (error "Load blog-password.el after defining the blog backend"))
  (cl-pushnew '(:blog-password "PASSWORD" nil nil t)
              (org-export-backend-options backend) :test #'equal))
(add-to-list 'org-export-global-macros
             (cons "lock" (concat "@@html:" cw/blog-password--lock "@@")))
(advice-add 'cw/blog-template :around #'cw/blog-password--template)
(advice-add 'cw/blog-publish-sitemap-dated-entry :around #'cw/blog-password--sitemap-entry)
(advice-add 'cw/blog-get-abstract :around #'cw/blog-password--abstract)
(advice-add 'cw/blog-publish-rss-entry :around #'cw/blog-password--rss-entry)

(provide 'blog-password)
;;; blog-password.el ends here
