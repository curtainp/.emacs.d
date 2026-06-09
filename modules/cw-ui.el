;;;  -*- lexical-binding: t; -*-

;; copy from chiply
(defun cw-svg--crumb-target (str pos text)
  "Return a buffer position/marker the crumb at POS (text TEXT) in STR points to.
Reads the target breadcrumb already stashed on the crumb: `breadcrumb-region'
or `org-imenu-marker' on the crumb itself, else the crumb's entry in the
`breadcrumb-siblings' alist (matched by TEXT).  Returns nil when no target is
discoverable (e.g. lsp crumbs, whose own handler we keep instead)."
  (let ((reg  (get-text-property pos 'breadcrumb-region str))
        (om   (get-text-property pos 'org-imenu-marker str))
        (sibs (get-text-property pos 'breadcrumb-siblings str)))
    (cond
     ((consp reg) (car reg))                       ; (start . end) -> start
     ((markerp om) om)
     ((and (listp sibs) text)
      (let ((hit (assoc text (mapcar (lambda (e)
                                       (cons (and (stringp (car-safe e))
                                                  (substring-no-properties (car e)))
                                             (cdr-safe e)))
                                     sibs))))
        (let ((tgt (cdr hit)))
          (cond ((markerp tgt) tgt)
                ((numberp tgt) tgt)
                ((overlayp tgt) (overlay-start tgt)))))))))

(defun cw-svg--crumb-jump (target)
  "Return an interactive command that navigates to TARGET (a marker or position).
Selects TARGET's window/buffer, pushes the mark, moves point and reveals it
\(unfolding in org), so a crumb click goes straight there -- no completing-read."
  (lambda ()
    (interactive)
    (let* ((m (if (markerp target) target nil))
           (buf (if m (marker-buffer m) (current-buffer)))
           (pos (if m (marker-position m) target)))
      (when (and buf pos (buffer-live-p buf))
        (let ((win (get-buffer-window buf)))
          (if win (select-window win)
            (pop-to-buffer buf)))
        (push-mark)
        (goto-char pos)
        (cond ((derived-mode-p 'org-mode)
               (ignore-errors (org-fold-show-context))
               (ignore-errors (org-fold-show-entry)))
              ((bound-and-true-p outline-minor-mode)
               (ignore-errors (outline-show-entry))))
        (recenter)))))

(defun cw-svg-segs-from-propertized (str id-key)
  "Split propertized STR into an svg-line `:svg-segs' group of clickable crumbs.
Builds on `svg-line-map-string-regions' (the package's region splitter +
mouse-1 handler extractor): each region carrying a mouse-1 keymap becomes an
interactive segment, regions without one stay plain text.  For a crumb whose own
text properties name a target (org/imenu markers, `breadcrumb-region'/`-siblings')
we jump there DIRECTLY -- bypassing `breadcrumb-jump''s `completing-read' -- else
we fall back to the crumb's own handler (e.g. lsp-headerline's, already direct).
Only a left-click action: these handlers take (interactive \"e\") and read the
invoking mouse event, which a real header-line click supplies.  ID-KEY (with the
buffer, for per-window hover) namespaces the per-crumb hover ids.  Returns nil
for empty STR."
  (when (and (stringp str) (fboundp 'svg-line-map-string-regions) (> (length str) 0))
    (let ((idx 0) (buf (current-buffer)))
      (apply #'svg-line-segs
             (svg-line-map-string-regions
              str
              (lambda (text start handler help)
                (let* ((target (and handler (cw-svg--crumb-target str start text)))
                       (act (cond (target (cw-svg--crumb-jump target))
                                  (handler handler))))
                  (if (and act (> (length (string-trim text)) 0))
                      (progn
                        (setq idx (1+ idx))
                        (svg-line-seg
                         text
                         :id (list id-key buf idx)
                         ;; clean help -- for a direct jump use the crumb text;
                         ;; else the handler's own first help line (properties
                         ;; stripped, since breadcrumb stuffs its sibling tree there)
                         :help (if target
                                   (concat "go to " (string-trim text))
                                 (if (stringp help)
                                     (substring-no-properties
                                      (car (split-string help "\n")))
                                   (format "%s" (string-trim text))))
                         :action-help (if target "jump" "open")
                         :action act))
                    text))))))))


;;; header-line
(defvar cw-header-line-svg-crumbs-format
  '((:eval
     (cond
      ((and (boundp 'lsp-mode) lsp-mode)
       (window-parameter nil 'lsp-headerline--string))
      ((derived-mode-p 'org-mode)
       ;; Prefer breadcrumb's imenu crumbs (each carries a clickable keymap our
       ;; header-line harvests) over `org-display-outline-path' (no per-crumb
       ;; keymap, so it would render as plain, non-clickable text).
       (if (fboundp 'breadcrumb-imenu-crumbs)
           (breadcrumb-imenu-crumbs)
         (propertize
          (or (ignore-errors (org-display-outline-path nil t "/" t)) "/")
          'face '(:height 0.8))))
      ((or (equal major-mode 'jsonian-mode))
       (concat (jsonian--display-path (jsonian-path))))
      ((or (equal major-mode 'docker-compose-mode)
           (equal major-mode 'yaml-mode))
       (concat (jpt-yaml-path-to-point)))
      (t (when (fboundp 'breadcrumb-imenu-crumbs) (breadcrumb-imenu-crumbs))))))
  "Mode-line construct for header-line (lsp / org / imenu crumbs).")

(defun cw-header-line-svg--line ()
  (cw-svg-segs-from-propertized
   (format-mode-line cw-header-line-svg-crumbs-format) 'hl1))

(provide 'cw-ui)
