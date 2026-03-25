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

(use-package ox
  :straight nil
  :config
  (setq org-export-with-priority t
        org-export-with-toc 4
        org-export-time-stamp-file nil  ;; Don't generate timestamp comment
        org-export-with-section-numbers nil
        org-export-with-planning t
        org-export-with-special-strings nil))

(use-package htmlize
  :straight t
  :defer t)

(use-package ox-html
  :straight nil
  :config
  (setq org-html-metadata-timestamp-format "%Y-%m-%d"
        org-html-head-include-default-style nil
        org-html-htmlize-output-type 'css
        org-html-validation-link nil
        org-html-prefer-user-labels t
        org-html-head-include-scripts t
        org-html-wrap-src-lines nil
        org-html-checkbox-type 'html
        org-html-checkbox-types '((unicode
                                   (on . "&#x2611;")
                                   (off . "&#x2610;")
                                   (trans . "&#x2610;"))
                                  (ascii
                                   (on . "<code>[X]</code>")
                                   (off . "<code>[&#xa0;]</code>")
                                   (trans . "<code>[-]</code>"))
                                  (html
                                   (on . "<input type='checkbox' checked='checked'onclick=\"return false;\"/>")
                                   (off . "<input type='checkbox'onclick=\"return false;\"/>")
                                   (trans . "<input type='checkbox'onclick=\"return false;\"/>")))
        org-html-link-home ""
        org-html-link-up ""
        org-html-postamble nil))

(use-package org-count-words
  :straight (:host github :repo "Elilif/org-count-words"))

(use-package ox-rss
  :straight t
  :config
  (defvar cw/blog-tags nil)
  (defconst cw/blog-draft-tag "draft")
  (defconst cw/blog-draft-truthy-values '("t" "true" "yes"))
  (defconst cw/blog-pinned-truthy-values '("t" "true" "yes" "1"))
  (defconst cw/blog-pin-macro "{{{pin()}}}")

  (defun cw/blog--draft-value-p (value)
    "Return non-nil when VALUE should mark a post as draft."
    (and (stringp value)
         (not (null (member (downcase (string-trim value))
                            cw/blog-draft-truthy-values)))))

  (defun cw/blog--pinned-value-p (value)
    "Return non-nil when VALUE should mark a post as pinned."
    (and (stringp value)
         (not (null (member (downcase (string-trim value))
                            cw/blog-pinned-truthy-values)))))

  (defun cw/blog--filetags-from-keywords (values)
    "Extract document tags from FILETAGS keyword VALUES."
    (cl-loop for value in values
             append (split-string value "[:[:space:]]+" t)))

  (defun cw/blog--compute-draft-p (file)
    "Return non-nil when FILE is marked as a draft."
    (when (and (file-readable-p file) (not (directory-name-p file)))
      (let ((org-inhibit-startup t))
        (org-with-file-buffer file
          (let* ((keywords (org-collect-keywords '("DRAFT" "FILETAGS")))
                 (draft-values (cdr (assoc "DRAFT" keywords)))
                 (filetags (cdr (assoc "FILETAGS" keywords))))
            (or (cl-some #'cw/blog--draft-value-p draft-values)
                (cl-some
                 (lambda (tag)
                   (string-equal (downcase tag) cw/blog-draft-tag))
                 (cw/blog--filetags-from-keywords filetags))))))))

  (defun cw/blog--compute-pinned-p (file)
    "Return non-nil when FILE is marked as pinned."
    (when (and (file-readable-p file) (not (directory-name-p file)))
      (let ((org-inhibit-startup t))
        (org-with-file-buffer file
          (let* ((keywords (org-collect-keywords '("PINNED")))
                 (pinned-values (cdr (assoc "PINNED" keywords))))
            (cl-some #'cw/blog--pinned-value-p pinned-values))))))

  (defun cw/blog-draft-p (file project)
    "Return non-nil when FILE in PROJECT should be skipped from publishing."
    (let* ((file (org-publish--expand-file-name file project))
           (project-name (car project))
           (missing (make-symbol "cw/blog-draft-missing"))
           (cached (org-publish-cache-get-file-property
                    file :cw-draft missing t project-name)))
      (if (eq cached missing)
          (org-publish-cache-set-file-property
           file :cw-draft (cw/blog--compute-draft-p file) project-name)
        cached)))

  (defun cw/blog-pinned-p (file project)
    "Return non-nil when FILE in PROJECT should be pinned on the homepage."
    (let* ((file (org-publish--expand-file-name file project))
           (project-name (car project))
           (missing (make-symbol "cw/blog-pinned-missing"))
           (cached (org-publish-cache-get-file-property
                    file :cw-pinned missing t project-name)))
      (if (eq cached missing)
          (org-publish-cache-set-file-property
           file :cw-pinned (cw/blog--compute-pinned-p file) project-name)
        cached)))

  (defun cw/org-publish-get-base-files (orig-fun project)
    "Filter draft posts from PROJECT before publishing."
    (let ((files (funcall orig-fun project)))
      (if (org-publish-property :cw-exclude-drafts project)
          (cl-remove-if (lambda (file)
                          (cw/blog-draft-p file project))
                        files)
        files)))

  (defun cw/blog-publish-sitemap-dated-entry (entry _style project)
    (let* ((file (org-publish--expand-file-name entry project))
           (parsed-title (org-publish-find-property file :title project))
           (title
            (if parsed-title
                (org-no-properties
                 (org-element-interpret-data parsed-title))
              (file-name-nondirectory (file-name-sans-extension file))))
           (tags (org-publish-find-property file :filetags project))
           (tags-string (mapconcat
                         (lambda (tag)
                           (concat "#" tag))
                         tags " ")))
      (dolist (tag tags)
        (cl-pushnew tag cw/blog-tags :test #'string=))
      (org-publish-cache-set-file-property file :title title)
      (if (= (length title) 0)
          (format "%s*" entry)
        (format "%s{{{timestamp(%s)}}}    [[file:%s][%s]] {{{tags(%s)}}}"
                (if (cw/blog-pinned-p file project)
                    cw/blog-pin-macro
                  "")
                (format-time-string
                 org-html-metadata-timestamp-format
                 (cw/org-publish-find-date file project))
                (concat "articles/" entry)
                title
                tags-string))))
  (defun cw/org-publish-find-date (file project)
    "Find the date of FILE in PROJECT.
This function assumes FILE is either a directory or an Org file.
If FILE is an Org file and provides a DATE keyword use it.  In
any other case use the file system's modification time.  Return
time in `current-time' format."
    (let ((file (org-publish--expand-file-name file project)))
      (or (org-publish-cache-get-file-property file :date nil t)
          (org-publish-cache-set-file-property
           file :date
           (if (file-directory-p file)
               (file-attribute-modification-time (file-attributes file))
             (let ((date (org-publish-find-property file :date project)))
               ;; DATE is a secondary string.  If it contains
               ;; a time-stamp, convert it to internal format.
               ;; Otherwise, use FILE modification time.
               (cond ((let ((ts (and (consp date) (assq 'timestamp date))))
                        (and ts
                             (let ((value (org-element-interpret-data ts)))
                               (and (org-string-nw-p value)
                                    (org-time-string-to-time value))))))
                     (date
                      (org-time-string-to-time (car date)))
                     ((file-exists-p file)
                      (file-attribute-modification-time (file-attributes file)))
                     (t (error "No such file: \"%s\"" file)))))))))

  (defun cw/blog-source-file-for-html (file)
    "Return the Org source file for published article FILE, or nil."
    (let ((articles-dir (expand-file-name "articles" cw/blog-publish-dir)))
      (when (file-in-directory-p file articles-dir)
        (expand-file-name
         (concat (file-name-sans-extension
                  (file-relative-name file articles-dir))
                 ".org")
         cw/blog-base-dir))))

  (defun cw/blog-draft-html-p (file)
    "Return non-nil when published article FILE was generated from a draft post."
    (when-let* ((project (assoc "blog articles" org-publish-project-alist))
                (source-file (cw/blog-source-file-for-html file))
                ((file-readable-p source-file)))
      (cw/blog-draft-p source-file project)))

  (defun cw/blog-lastmod-time-for-html (file)
    "Return the timestamp to use as sitemap lastmod for FILE."
    (if-let* ((source-file (cw/blog-source-file-for-html file))
              ((file-readable-p source-file)))
        (file-attribute-modification-time (file-attributes source-file))
      (file-attribute-modification-time (file-attributes file))))

  (defun cw/blog-generate-sitemap (&optional _project)
    "Generate a sitemap.xml file for PROJTCT."
    (let* ((sitemap-path (file-name-concat cw/blog-publish-dir "sitemap.xml"))
           (base-url "https://thefuzzdog.top/")
           (files (cl-remove-if #'cw/blog-draft-html-p
                                (directory-files-recursively cw/blog-publish-dir ".html")))
           (sitemap-buffer (generate-new-buffer "*sitemap*")))
      (with-current-buffer sitemap-buffer
        (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">
")
        (dolist (file files)
          (insert
           (format "<url>\n<loc>%s</loc>\n<lastmod>%s</lastmod>\n</url>\n"
                   (concat base-url (file-relative-name file cw/blog-publish-dir))
                   (format-time-string "%Y-%m-%dT%H:%M:%S+08:00"
                                       (cw/blog-lastmod-time-for-html file)))))
        (insert "</urlset>")

        (write-region (point-min) (point-max) sitemap-path nil 3)
        (kill-buffer sitemap-buffer))))

  (defun cw/blog-move-sitemap (project)
    (let* ((publishing-directory (plist-get project :publishing-directory))
           (sitemap (file-name-with-extension cw/blog-sitemap "html"))
           (orig-file (expand-file-name sitemap publishing-directory))
           (target-file (expand-file-name
                         sitemap
                         (file-name-directory publishing-directory))))
      (rename-file orig-file target-file t)))

  
(defun cw/blog-publish-completion (project)
  (cw/blog-move-sitemap project)
  (cw/blog-generate-sitemap)
  (setq org-html-head-extra ""
        cw/blog-tags nil))

(defun cw/blog-publish-rss-sitemap (title list)
  "Generate a sitemap of posts that is exported as a RSS feed.
TITLE is the title of the RSS feed.  LIST is an internal
representation for the files to include.  PROJECT is the current
project."
  (concat
   "#+TITLE: " title
   "\n\n"
   (org-list-to-subtree list)))

(defun cw/blog-get-abstract (file)
  "Get the contents of abstract block in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((beg (re-search-forward "^#\\+begin_abstract\n" nil t))
          (end (progn (re-search-forward "^#\\+end_abstract$" nil t)
                      (match-beginning 0))))
      (if beg
          (buffer-substring beg end)
        ""))))

(defun cw/blog-publish-rss-entry (entry _style project)
  "Format ENTRY for the posts RSS feed in PROJECT."
  (let* ((file (org-publish--expand-file-name entry project))
         (abstract (cw/blog-get-abstract file))
         (parsed-title (org-publish-find-property file :title project))
         (title
          (if parsed-title
              (org-no-properties
               (org-element-interpret-data parsed-title))
            (file-name-nondirectory (file-name-sans-extension file))))
         (root (org-publish-property :html-link-home project))
         (link (concat
                "articles/"
                (file-name-sans-extension entry) ".html"))
         (pubdate (format-time-string
                   (cdr org-time-stamp-formats)
                   (cw/org-publish-find-date file project))))
    (org-publish-cache-set-file-property file :title title)
    (format "%s
:properties:
:rss_permalink: %s
:pubdate: %s
:end:\n%s\n[[%s][Read More]]"
            title
            link
            pubdate
            abstract
            (concat
             root
             link))))

  (defun cw/kill-sitemap-buffer (project)
    (let* ((sitemap-filename (plist-get project :sitemap-filename))
           (base-dir (plist-get project :base-directory))
           (sitemap-filepath (expand-file-name sitemap-filename base-dir)))
      (when-let* ((sitemap-buffer (find-buffer-visiting sitemap-filepath)))
        (kill-buffer sitemap-buffer))))

  (defun cw/blog-publish-rss-feed (plist filename dir)
  "Publish PLIST to Rss when FILENAME is rss.org.
DIR is the location of the output."
  (if (equal "rss.org" (file-name-nondirectory filename))
      (org-publish-org-to
       'rss filename (concat "." org-rss-extension) plist dir)))

  (defun cw/blog-sitemap-item-pinned-p (item)
    "Return non-nil when sitemap ITEM is marked as pinned."
    (let ((text (car-safe item)))
      (and (stringp text)
           (string-prefix-p cw/blog-pin-macro text))))

  (defun cw/blog-strip-pin-marker (text)
    "Remove the pin marker from TEXT."
    (if (string-prefix-p cw/blog-pin-macro text)
        (substring text (length cw/blog-pin-macro))
      text))

  (defun cw/blog-sitemap-strip-pin-markers (node)
    "Return sitemap NODE without pin markers."
    (cond
     ((stringp node) (cw/blog-strip-pin-marker node))
     ((consp node) (mapcar #'cw/blog-sitemap-strip-pin-markers node))
     (t node)))

  (defun cw/blog-sitemap-prioritize-pinned-items (list)
    "Return LIST with pinned entries before regular entries."
    (let (pinned regular)
      (dolist (item (cdr list))
        (if (cw/blog-sitemap-item-pinned-p item)
            (push item pinned)
          (push item regular)))
      (cons (car list)
            (append (nreverse pinned)
                    (nreverse regular)))))

  (defun cw/blog-write-tags-page (title list)
    "Generate tags.org in the blog base directory."
    (let* ((tags-path (expand-file-name "tags.org" cw/blog-base-dir))
           (filter-css-lines
            (append
             (list "<style>")
             (mapcar
              (lambda (tag)
                (format ".content:has([value=\"%s\"]:checked) li:has([data-tags~=\"%s\"]){display: list-item;}"
                        tag (concat "#" tag)))
              cw/blog-tags)
             (list "</style>")))
           (filter-html
            (format "<section class=\"filter\">\n%s\n%s</section>"
                    (mapconcat
                     (lambda (tag)
                       (format "<label class=\"category\">
<input type=\"radio\" name=\"tag\" value=\"%s\"/>
<span>%s</span>
</label>"
                               tag tag))
                     cw/blog-tags "\n")
                    "")))
      (with-temp-file tags-path
        (insert "#+TITLE: " title "\n"
                "#+DATE: 2026-03-12\n"
                "#+OPTIONS: title:nil\n"
                "#+OPTIONS: ^:nil\n"
                (mapconcat (lambda (line)
                             (concat "#+HTML_HEAD_EXTRA: " line))
                           filter-css-lines "\n")
                "\n"
                "#+BEGIN_EXPORT html\n"
                filter-html "\n"
                "#+END_EXPORT\n"
                (org-list-to-org list)))))

  (defun cw/blog-publish-sitemap (title list)
    "Generate the sitemap with title."
    (let ((homepage-list (cw/blog-sitemap-prioritize-pinned-items list))
          (tags-list (cw/blog-sitemap-strip-pin-markers list)))
      (cw/blog-write-tags-page "Tags" tags-list)
      (concat "#+TITLE: " title
              "\n"
              "#+DATE: 2026-03-12"
              "\n"
              "#+HTML_HEAD_EXTRA: <style>.content li:has(.tags){display: list-item;}</style>\n"
              (org-list-to-org homepage-list))))

  (defun cw/org-blog-add-noweb-ref (data backend _info)
    (when (eq backend 'blog)
      (replace-regexp-in-string
       "&lt;&lt;\\(.*?\\)&gt;&gt;"
       "<a href=\"#\\1\">\\1</a>"
       data nil nil 1)))

  (defun cw/org-blog-id-filter (data backend _info)
    "Remove random ID attributes generated by Org."
    (when (memq backend '(blog rss))
      (replace-regexp-in-string
       " id=\"[[:alpha:]-]*org[[:alnum:]]\\{7\\}\""
       ""
       data t)))

  (defun cw/org-export-src-babel-duplicate (backend)
    "Duplicate every src babels in the current buffer.

add \":noweb yes\" to duplicated src babels."
    (when (eq backend 'blog)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-babel-src-block-regexp nil t)
          (let* ((end (copy-marker (match-end 0)))
                 (string (match-string 0))
                 (block (org-element-at-point))
                 (code (org-element-property :value block))
                 (params (org-element-property :parameters block)))
            (when (eq (org-element-type block) 'src-block)
              (goto-char (org-element-property :begin block))
              (insert "#+begin_multilang")
              (insert "\n")
              (goto-char end)
              (insert "\n")
              (when (and (string-match-p (org-babel-noweb-wrap) code)
                         (not (string-match-p ":noweb" params)))
                (insert string)
                (save-excursion
                  (goto-char (1+ end))
                  (end-of-line)
                  (insert " :noweb yes"))
                (insert "\n"))
              (insert "#+end_multilang")
              (insert "\n")))))))

  (defun cw/org-export-add-custom-id (backend)
    "Add CUSTOM-ID to headlines which dosen't have it."
    (when (eq backend 'blog)
      (org-map-entries
       (lambda ()
         (unless (org-entry-get (point) "CUSTOM_ID")
           (let* ((headline-name (replace-regexp-in-string
                                  " " "-"
                                  (nth 4 (org-heading-components)))))
             (org-entry-put (point) "CUSTOM_ID" headline-name)))))))

  (defun cw/blog-src-block (src-block _contents info)
    "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
    (if (org-export-read-attribute :attr_html src-block :textarea)
        (org-html--textarea-block src-block)
      (let* ((lang (org-element-property :language src-block))
             (code (org-html-format-code src-block info))
             (label (let ((lbl (org-html--reference src-block info t)))
                      (if lbl (format " id=\"%s\"" lbl) "")))
             (klipsify  (and  (plist-get info :html-klipsify-src)
                              (member lang '("javascript" "js"
                                             "ruby" "scheme" "clojure" "php" "html")))))
        (if (not lang) (format "<pre class=\"example\"%s>\n%s</pre>" label code)
          (format "<div class=\"org-src-container\">\n%s%s\n</div>"
                  ;; Build caption.
                  (let ((caption (or (org-export-get-caption src-block)
                                     (org-element-property :name src-block))))
                    (if (not caption) ""
                      (let ((listing-number
                             (format
                              "<span class=\"listing-number\">%s </span>"
                              (format
                               (org-html--translate "Listing %d:" info)
                               (org-export-get-ordinal
                                src-block info nil #'org-html--has-caption-p)))))
                        (format "<label class=\"org-src-name\">%s%s</label>"
                                listing-number
                                (org-trim (org-export-data caption info))))))
                  ;; Contents.
                  (if klipsify
                      (format "<pre><code class=\"src src-%s\"%s%s>%s</code></pre>"
                              lang
                              label
                              (if (string= lang "html")
                                  " data-editor-type=\"html\""
                                "")
                              code)
                    (format "<pre class=\"src src-%s\"%s>%s</pre>"
                            lang label code)))))))

  (defun cw/blog-link (link desc info)
    "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
    (let* ((html-ext (plist-get info :html-extension))
           (dot (when (> (length html-ext) 0) "."))
           (link-org-files-as-html-maybe
            (lambda (raw-path info)
              ;; Treat links to `file.org' as links to `file.html', if
              ;; needed.  See `org-html-link-org-files-as-html'.
              (save-match-data
                (cond
                 ((and (plist-get info :html-link-org-files-as-html)
                       (let ((case-fold-search t))
                         (string-match "\\(.+\\)\\.org\\(?:\\.gpg\\)?$" raw-path)))
                  (concat (match-string 1 raw-path) dot html-ext))
                 (t raw-path)))))
           (type (org-element-property :type link))
           (raw-path (org-element-property :path link))
           ;; Ensure DESC really exists, or set it to nil.
           (desc (org-string-nw-p desc))
           (path
            (cond
             ((member type '("http" "https" "ftp" "mailto" "news"))
              (url-encode-url (concat type ":" raw-path)))
             ((string= "file" type)
              ;; During publishing, turn absolute file names belonging
              ;; to base directory into relative file names.  Otherwise,
              ;; append "file" protocol to absolute file name.
              (setq raw-path
                    (org-export-file-uri
                     (org-publish-file-relative-name raw-path info)))
              ;; Possibly append `:html-link-home' to relative file
              ;; name.
              (let ((home (and (plist-get info :html-link-home)
                               (org-trim (plist-get info :html-link-home)))))
                (when (and home
                           (plist-get info :html-link-use-abs-url)
                           (file-name-absolute-p raw-path))
                  (setq raw-path (concat (file-name-as-directory home) raw-path))))
              ;; Maybe turn ".org" into ".html".
              (setq raw-path (funcall link-org-files-as-html-maybe raw-path info))
              ;; Add search option, if any.  A search option can be
              ;; relative to a custom-id, a headline title, a name or
              ;; a target.
              (let ((option (org-element-property :search-option link)))
                (if (not option) raw-path
                  (let ((path (org-element-property :path link)))
                    (concat raw-path
                            "#"
                            (org-publish-resolve-external-link option path t))))))
             (t raw-path)))
           (attributes-plist
            (org-combine-plists
             ;; Extract attributes from parent's paragraph.  HACK: Only
             ;; do this for the first link in parent (inner image link
             ;; for inline images).  This is needed as long as
             ;; attributes cannot be set on a per link basis.
             (let* ((parent (org-export-get-parent-element link))
                    (link (let ((container (org-export-get-parent link)))
                            (if (and (eq 'link (org-element-type container))
                                     (org-html-inline-image-p link info))
                                container
                              link))))
               (and (eq link (org-element-map parent 'link #'identity info t))
                    (org-export-read-attribute :attr_html parent)))
             ;; Also add attributes from link itself.  Currently, those
             ;; need to be added programmatically before `org-html-link'
             ;; is invoked, for example, by backends building upon HTML
             ;; export.
             (org-export-read-attribute :attr_html link)))
           (attributes
            (let ((attr (org-html--make-attribute-string attributes-plist)))
              (if (org-string-nw-p attr) (concat " " attr) ""))))
      (cond
       ;; Link type is handled by a special function.
       ((org-export-custom-protocol-maybe link desc 'html info))
       ;; Image file.
       ((and (plist-get info :html-inline-images)
             (org-export-inline-image-p
              link (plist-get info :html-inline-image-rules)))
        (org-html--format-image path attributes-plist info))
       ;; Radio target: Transcode target's contents and use them as
       ;; link's description.
       ((string= type "radio")
        (let ((destination (org-export-resolve-radio-link link info)))
          (if (not destination) desc
            (format "<a href=\"#%s\"%s>%s</a>"
                    (org-html--reference destination info)
                    attributes
                    desc))))
       ;; Links pointing to a headline: Find destination and build
       ;; appropriate referencing command.
       ((member type '("custom-id" "fuzzy" "id"))
        (let ((destination (if (string= type "fuzzy")
                               (org-export-resolve-fuzzy-link link info)
                             (org-export-resolve-id-link link info))))
          (pcase (org-element-type destination)
            ;; ID link points to an external file.
            (`plain-text
             (let ((fragment (concat "ID-" path))
                   ;; Treat links to ".org" files as ".html", if needed.
                   (path (funcall link-org-files-as-html-maybe
                                  destination info)))
               (format "<a href=\"%s#%s\"%s>%s</a>"
                       path fragment attributes (or desc destination))))
            ;; Fuzzy link points nowhere.
            (`nil
             (format "<i>%s</i>"
                     (or desc
                         (org-export-data
                          (org-element-property :raw-link link) info))))
            ;; Link points to a headline.
            (`headline
             (let ((href (org-html--reference destination info))
                   ;; What description to use?
                   (desc
                    ;; Case 1: Headline is numbered and LINK has no
                    ;; description.  Display section number.
                    (if (and (org-export-numbered-headline-p destination info)
                             (not desc))
                        (mapconcat #'number-to-string
                                   (org-export-get-headline-number
                                    destination info) ".")
                      ;; Case 2: Either the headline is un-numbered or
                      ;; LINK has a custom description.  Display LINK's
                      ;; description or headline's title.
                      (or desc
                          (org-export-data
                           (org-element-property :title destination) info)))))
               (format "<a href=\"#%s\"%s>%s</a>" href attributes desc)))
            ;; Fuzzy link points to a target or an element.
            (_
             (if (and destination
                      (memq (plist-get info :with-latex) '(mathjax t))
                      (eq 'latex-environment (org-element-type destination))
                      (eq 'math (org-latex--environment-type destination)))
                 ;; Caption and labels are introduced within LaTeX
                 ;; environment.  Use "ref" or "eqref" macro, depending on user
                 ;; preference to refer to those in the document.
                 (format (plist-get info :html-equation-reference-format)
                         (org-html--reference destination info))
               (let* ((ref (org-html--reference destination info))
                      (org-html-standalone-image-predicate
                       #'org-html--has-caption-p)
                      (counter-predicate
                       (if (eq 'latex-environment (org-element-type destination))
                           #'org-html--math-environment-p
                         #'org-html--has-caption-p))
                      (number
                       (cond
                        (desc nil)
                        ((org-html-standalone-image-p destination info)
                         (org-export-get-ordinal
                          (org-element-map destination 'link #'identity info t)
                          info '(link) 'org-html-standalone-image-p))
                        (t (org-export-get-ordinal
                            destination info nil counter-predicate))))
                      (desc
                       (cond (desc)
                             ((not number) "No description for this link")
                             ((numberp number) (number-to-string number))
                             (t (mapconcat #'number-to-string number ".")))))
                 (format "<a href=\"#%s\"%s>%s</a>" ref attributes desc)))))))
       ;; Coderef: replace link with the reference name or the
       ;; equivalent line number.
       ((string= type "coderef")
        (let ((fragment (concat "coderef-" (org-html-encode-plain-text path))))
          (format "<a href=\"#%s\" %s%s>%s</a>"
                  fragment
                  (format "class=\"coderef\" onmouseover=\"CodeHighlightOn(this, \
'%s');\" onmouseout=\"CodeHighlightOff(this, '%s');\""
                          fragment fragment)
                  attributes
                  (format (org-export-get-coderef-format path desc)
                          (org-export-resolve-coderef path info)))))
       ;; External link with a description part.
       ((and path desc)
        (format "<a href=\"%s\"%s>%s</a>"
                (org-html-encode-plain-text path)
                attributes
                desc))
       ;; External link without a description part.
       (path
        (let ((path (org-html-encode-plain-text path)))
          (format "<a href=\"%s\"%s>%s</a>" path attributes path)))
       ;; No path, only description.  Try to do something useful.
       (t
        (format "<i>%s</i>" desc)))))

  (defun cw/org-publish-get-wordcount (info)
    "Get the number of words in FILENAME."
    (let ((filename (plist-get info :input-file))
          (pub-dir (file-name-as-directory (plist-get info :publishing-directory)))
          (pub-func (plist-get info :publishing-function)))
      (if (org-publish-cache-file-needs-publishing filename pub-dir pub-func)
          (cw/org-publish-update-wordcount filename)
        (or (org-publish-cache-get (sha1 filename))
            (cw/org-publish-update-wordcount filename)))))

  (defun cw/org-publish-update-wordcount (filename)
    (when (and (file-readable-p filename) (not (directory-name-p filename)))
      (let* ((org-inhibit-startup t)
             (visiting (find-buffer-visiting filename))
             (buffer (or visiting (find-file-noselect filename)))
             wordcount)
        (setq wordcount (unwind-protect
                            (with-current-buffer buffer
                              (org-count-words-buffer))
                          (unless visiting (kill-buffer buffer))))
        (org-publish-cache-set (sha1 filename) wordcount)
        (unless visiting (kill-buffer buffer))
        wordcount)))

  (defvar cw/blog-status-format "<span><i class='bx bx-calendar'></i>
<span>%d</span></span>\n<span><i class='bx bx-edit'></i><span>%C</span></span>")

  (defun cw/blog-build-article-status (info)
    (let ((input-file (file-name-nondirectory (plist-get info :input-file))))
      (unless (or (string-equal input-file cw/blog-sitemap)
                  (string-equal input-file "tags.org"))
        (let ((spec (org-html-format-spec info)))
          (concat
           "<div class=\"post-status\">"
           (format-spec cw/blog-status-format spec)
           (format "<span><i class='bx bxs-hourglass'></i>%s Words</span>" (cw/org-publish-get-wordcount info))
           "</div>")))))

  (defvar cw/blog-giscus-script "<script src=\"https://giscus.app/client.js\"
          data-repo=\"curtainp/blog-comments\"
          data-repo-id=\"R_kgDORfx2KA\"
          data-category=\"Announcements\"
          data-category-id=\"DIC_kwDORfx2KM4C3y4H\"
          data-mapping=\"pathname\"
          data-strict=\"0\"
          data-reactions-enabled=\"1\"
          data-emit-metadata=\"0\"
          data-input-position=\"top\"
          data-theme=\"dark_tritanopia\"
          data-lang=\"en\"
          crossorigin=\"anonymous\"
          async>
  </script>")

  (defun cw/blog-build-giscus (info)
    (let ((input-file (file-name-nondirectory (plist-get info :input-file))))
      (unless (or (string-equal input-file cw/blog-sitemap)
                  (string-equal input-file "tags.org"))
        cw/blog-giscus-script)))

  (defun cw/blog-template (contents info)
    "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
    (concat
     (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
       (let* ((xml-declaration (plist-get info :html-xml-declaration))
              (decl (or (and (stringp xml-declaration) xml-declaration)
                        (cdr (assoc (plist-get info :html-extension)
                                    xml-declaration))
                        (cdr (assoc "html" xml-declaration))
                        "")))
         (when (not (or (not decl) (string= "" decl)))
           (format "%s\n"
                   (format decl
                           (or (and org-html-coding-system
                                    ;; FIXME: Use Emacs 22 style here, see `coding-system-get'.
                                    (coding-system-get org-html-coding-system 'mime-charset))
                               "iso-8859-1"))))))
     (org-html-doctype info)
     "\n"
     (concat "<html"
             (cond ((org-html-xhtml-p info)
                    (format
                     " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
                     (plist-get info :language) (plist-get info :language)))
                   ((org-html-html5-p info)
                    (format " lang=\"%s\"" (plist-get info :language))))
             ">\n")
     "<head>\n"
     (org-html--build-meta-info info)
     (org-html--build-head info)
     (org-html--build-mathjax-config info)
     "</head>\n"
     "<body>\n"
     (let ((link-up (org-trim (plist-get info :html-link-up)))
           (link-home (org-trim (plist-get info :html-link-home))))
       (unless (and (string= link-up "") (string= link-home ""))
         (format (plist-get info :html-home/up-format)
                 (or link-up link-home)
                 (or link-home link-up))))
     ;; Preamble.
     (org-html--build-pre/postamble 'preamble info)
     ;; Document contents.
     (let ((div (assq 'content (plist-get info :html-divs))))
       (format "<%s id=\"%s\" class=\"%s\">\n"
               (nth 1 div)
               (nth 2 div)
               (plist-get info :html-content-class)))
     ;; Document title.
     (when (plist-get info :with-title)
       (let ((title (and (plist-get info :with-title)
                         (plist-get info :title)))
             (subtitle (plist-get info :subtitle))
             (html5-fancy (org-html--html5-fancy-p info)))
         (when title
           (format
            (if html5-fancy
                "<header>\n<h1 class=\"title\">%s</h1>\n%s</header>"
              "<h1 class=\"title\">%s%s</h1>\n")
            (org-export-data title info)
            (if subtitle
                (format
                 (if html5-fancy
                     "<p class=\"subtitle\" role=\"doc-subtitle\">%s</p>\n"
                   (concat "\n" (org-html-close-tag "br" nil info) "\n"
                           "<span class=\"subtitle\">%s</span>\n"))
                 (org-export-data subtitle info))
              "")))))
     ;; add article status
     (cw/blog-build-article-status info)
     contents
     (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
     ;; gisus
     (cw/blog-build-giscus info)
     ;; Postamble.
     (org-html--build-pre/postamble 'postamble info)
     ;; Possibly use the Klipse library live code blocks.
     (when (plist-get info :html-klipsify-src)
       (concat "<script>" (plist-get info :html-klipse-selection-script)
               "</script><script src=\""
               org-html-klipse-js
               "\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\""
               org-html-klipse-css "\"/>"))
     ;; Closing document.
     "</body>\n</html>"))

  (defun cw/blog-footnote-reference (footnote-reference _contents info)
    "Transcode a FOOTNOTE-REFERENCE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
    (concat
     ;; Insert separator between two footnotes in a row.
     (let ((prev (org-export-get-previous-element footnote-reference info)))
       (when (eq (org-element-type prev) 'footnote-reference)
         (plist-get info :html-footnote-separator)))
     (let* ((n (org-export-get-footnote-number footnote-reference info))
            (id (format "fnr.%d%s"
                        n
                        (if (org-export-footnote-first-reference-p
                             footnote-reference info)
                            ""
                          ".100"))))
       (format
        (concat (plist-get info :html-footnote-format)
                "<input id=\"%s\" class=\"footref-toggle\" type=\"checkbox\">")
        (format "<label for=\"%s\" class=\"footref\">%s</label>"
                id n)
        id))))

  (defvar cw/blog-static-dir "~/Documents/org-blog/static/post-img/")
  (defun cw/blog-replace-static-path ()
    (save-excursion
      (org-element-cache-reset)
      (let ((static-dir (file-name-as-directory
                         (file-name-concat cw/blog-static-dir
                                           (file-name-base
                                            (buffer-file-name)))))
            (datum (org-element-parse-buffer))
            links)
        (unless (file-exists-p static-dir)
          (make-directory static-dir t))
        (org-element-map datum 'link
          (lambda (link)
            (when (or (and (string= (org-element-property :type link) "file")
                           (org-file-image-p (org-element-property :path link)))
                      (string= (org-element-property :type link) "video"))
              (let* ((beg (org-element-property :begin link))
                     (end (org-element-property :end link))
                     (old-file (org-element-property :path link))
                     (raw-link (org-element-property :raw-link link))
                     (new-file (file-name-concat
                                ".."
                                (file-relative-name
                                 static-dir
                                 cw/blog-publish-dir)
                                (file-name-nondirectory old-file)))
                     (new-link (org-link-make-string
                                (replace-regexp-in-string old-file new-file raw-link))))
                (unless (file-exists-p new-file)
                  (copy-file old-file new-file)
                  (push (list (copy-marker beg)
                              (copy-marker end)
                              new-link)
                        links))))))
        (when (directory-empty-p static-dir)
          (delete-directory static-dir))
        (dolist (link links)
          (let ((beg (nth 0 link))
                (end (nth 1 link))
                (new-link (nth 2 link)))
            (goto-char beg)
            (delete-region beg end)
            (insert new-link))))
      (save-buffer)))
  ;;;###autoload
  (defun cw/blog-publish-to-html (plist filename pub-dir)
    (with-current-buffer (or (find-buffer-visiting filename)
                             (find-file-noselect filename))
      (cw/blog-replace-static-path))
    (org-publish-org-to 'blog filename
                        (concat (when (> (length org-html-extension) 0) ".")
                                (or (plist-get plist :html-extension)
                                    org-html-extension
                                    "html"))
                        plist pub-dir))
  (defun org-video-link-export (path _desc backend)
    (let ((ext (file-name-extension path))
          (file-name (file-name-base path)))
      (cond
       ((org-export-derived-backend-p backend 'html)
        (format "<video preload='metadata' controls='controls'>
<source type='video/%s' src='%s' />
<a href='%s'>[VIDEO: %s]</a>
</video>" ext path path file-name))
       (t
        path))))
  (org-link-set-parameters "video" :export 'org-video-link-export)
  )

(use-package ox-publish
  :straight nil
  :after ob
  :after ox-rss
  :config
  (setq cw/blog-base-dir "~/Documents/org-blog/orgs"
        cw/blog-publish-dir "~/Documents/org-blog"
        cw/blog-sitemap "index.org"
        cw/blog-head "<link rel=\"icon\" href=\"/static/favicon.ico\">
<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/css/styles.css\"/>
<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/css/htmlize.css\" />
<link href=\"https://iosevka-webfonts.github.io/iosevka/Iosevka.css\" rel=\"stylesheet\" />
<link href=\"https://unpkg.com/boxicons@2.1.4/css/boxicons.min.css\" rel=\"stylesheet\">
<script src=\"/static/scripts/script.js\"></script>
<script src=\"/static/scripts/toc.js\"></script>
"
        cw/blog-preamble '(("en" "<nav class=\"nav\">
  <a href=\"/index.html\" class=\"button\">Home</a>
  <a href=\"/tags.html\" class=\"button\">Tags</a>
  <a href=\"/rss.xml\" class=\"button\">RSS</a>
</nav>
<hr>"))
        cw/blog-postamble '(("en" "<hr class=\"Solid\">
<div class=\"info\">
  <span>Creator: %c</span>
</div>"))
        org-publish-project-alist
        `(("blog articles"
           :base-directory ,cw/blog-base-dir
           :publishing-directory ,(expand-file-name "articles" cw/blog-publish-dir)
           :base-extension "org"
           :cw-exclude-drafts t
           :recursive nil
           :htmlized-source t
           :headline-levels 4
           :publishing-function cw/blog-publish-to-html
           :exclude "rss.org\\|tags.org"
           :auto-sitemap t
           :preparation-function cw/kill-sitemap-buffer
           :completion-function cw/blog-publish-completion
           :sitemap-filename ,cw/blog-sitemap
           :sitemap-title "Curtain's Blog"
           :sitemap-sort-files anti-chronologically
           :sitemap-function cw/blog-publish-sitemap
           :sitemap-format-entry cw/blog-publish-sitemap-dated-entry
           :html-head ,cw/blog-head
           :html-preamble t
           :html-preamble-format ,cw/blog-preamble
           :html-postamble t
           :author  "curtain"
           :email "Y3VydGFpbndrQHByb3Rvbi5tZQo="
           :html-postamble-format ,cw/blog-postamble
           :with-creator nil)
          ("blog tags"
           :base-directory ,cw/blog-base-dir
           :publishing-directory ,cw/blog-publish-dir
           :base-extension "none"
           :recursive nil
           :htmlized-source t
           :headline-levels 4
           :publishing-function cw/blog-publish-to-html
           :include ("tags.org")
           :exclude ".*"
           :html-head ,cw/blog-head
           :html-preamble t
           :html-preamble-format ,cw/blog-preamble
           :html-postamble t
           :author  "curtain"
           :email "Y3VydGFpbndrQHByb3Rvbi5tZQo="
           :html-postamble-format ,cw/blog-postamble
           :with-creator nil)
          ("blog rss"
           :preparation-function cw/kill-sitemap-buffer
           :publishing-directory ,cw/blog-publish-dir
           :base-directory ,cw/blog-base-dir
           :rss-extension "xml"
           :base-extension "org"
           :cw-exclude-drafts t
           :html-link-home "https://thefuzzdog.top/"
           :html-link-use-abs-url t
           :html-link-org-files-as-html t
           :include ("rss.org")
           :exclude ,cw/blog-sitemap
           :publishing-function cw/blog-publish-rss-feed
           :auto-sitemap t
           :sitemap-function cw/blog-publish-rss-sitemap
           :sitemap-title "Curtain's Blog"
           :sitemap-filename "rss.org"
           :author  "curtain"
           :email "Y3VydGFpbndrQHByb3Rvbi5tZQo="
           :sitemap-sort-files anti-chronologically
           :sitemap-format-entry cw/blog-publish-rss-entry)
          ("Curtain's Blog"
           :components ("blog articles" "blog tags" "blog rss"))
          ))
  (when (require 'ox)
    (add-to-list 'org-export-global-macros
                 '("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@"))
    (add-to-list 'org-export-global-macros
                 '("tags" . "@@html:<span class=\"tags\" data-tags=\"$1\"></span>@@"))
    (add-to-list 'org-export-global-macros
                 '("kbd" . "@@html:<kbd>$1</kbd>@@"))
    (add-to-list 'org-export-global-macros
                 '("pin" . "@@html:<span class=\"pinned-marker\" aria-hidden=\"true\"><i class=\"bx bxs-pin\"></i></span>@@")))
  (when (require 'ox-html)
    (org-export-define-derived-backend 'blog 'html
      :translate-alist '((src-block . cw/blog-src-block)
                         (footnote-reference . cw/blog-footnote-reference)
                         (template . cw/blog-template)
                         (link . cw/blog-link))))
  (advice-add #'org-publish-get-base-files :around #'cw/org-publish-get-base-files)
  (advice-add #'org-publish-find-date :override #'cw/org-publish-find-date)
  (add-hook 'org-export-before-processing-functions 'cw/org-export-src-babel-duplicate)
  (add-hook 'org-export-before-processing-functions 'cw/org-export-add-custom-id)
  (add-hook 'org-export-filter-src-block-functions 'cw/org-blog-add-noweb-ref)
  (add-hook 'org-export-filter-final-output-functions 'cw/org-blog-id-filter)

  )

(defun cw/blog--ensure-publish-deps ()
  "Load blog publishing dependencies on demand."
  (require 'ox)
  (require 'ox-html)
  (require 'htmlize)
  (require 'org-count-words)
  (require 'ox-rss)
  (require 'ox-publish))

;;;###autoload
(defun cw/publish-blog ()
  "Publish blog with auto-revert-mode temporarily disabled."
  (interactive)
  (cw/blog--ensure-publish-deps)
  (require 'autorevert)
  (let ((auto-revert-stop-on-user-input nil))
    (global-auto-revert-mode -1)
    (unwind-protect
        (org-publish "Curtain's Blog" nil)
      (global-auto-revert-mode 1))))

(provide 'init-notes)
