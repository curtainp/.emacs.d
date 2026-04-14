;;; cw-simple.el -*- lexical-binding: t -*-

;; copy from Protesilaos Stavrou

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))


(defgroup cw-simple ()
  "Generic utilities for improve built-in simple.el"
  :group 'editing)

(defcustom cw-simple-date-specifier "%F"
  "Date specifier for `format-time-string'.
Used by `cw-simple-insert-date'. '%F' is the ISO 8601 date format (like %-4Y-%m-%d)"
  :type 'string
  :group 'cw-simple)

(defcustom cw-simple-time-specifier "%R %z"
  "Time specifier for `format-time-string'.
Used by `cw-simple-insert-date'. '%R %z' is like %H:%M, %z is the numeric form."
  :type 'string
  :group 'cw-simple)

;;;###autoload
(defun cw-simple-indent-dwim ()
  "Indent the current defun in `prog-mode' or paragraph in `text-mode'."
  (interactive)
  (save-excursion
    (cond
     ((derived-mode-p 'prog-mode)
      (mark-defun))
     ((derived-mode-p 'text-mode)
      (mark-paragraph)))
    (indent-for-tab-command)
    (deactivate-mark)))

(defun cw-simple--mark (bounds)
  "Mark between BOUNDS as a cons cell of beginning and end positions."
  (push-mark (car bounds))
  (goto-char (cdr bounds))
  (activate-mark))

;;;###autoload
(defun cw-simple-mark-sexp ()
  "Mark symbolic expression at or near point. Repeat to extend the region
forward to the next symbolic expression."
  (interactive)
  (if (and (region-active-p)
           (eq last-command this-command))
      (ignore-errors (forward-sexp 1))
    (when-let* ((thing (cond
                        ((thing-at-point 'url) 'url)
                        ((thing-at-point 'sexp) 'sexp)
                        ((thing-at-point 'string) 'string)
                        ((thing-at-point 'word) 'word))))
      (cw-simple--mark (bounds-of-thing-at-point thing)))))

;;;###autoload
(defun cw-simple-keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.

- When a minibuffer is open, but not focused, close the minibuffer.
  For recursive minibuffer, make sure to only close one level of depth.

- When in a *Completions* or `special-mode' buffer (e.g. *Help* or *Message*),
  close it.

- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((and (derived-mode-p 'completion-list-mode 'special-mode)
         (not (one-window-p)))
    (quit-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

;;;###autoload
(defun cw-simple-delete-window-dwim ()
  "DWIM to delete the current THING. When there is more than one window,
THING is a window. When there are more one `tab-bar-mode' tabs, THING is
a tab. Else THING is frame if frames are more than one."
  (interactive)
  (cond
   ((length> (window-list) 1)
    (delete-window))
   ((and (featurep 'tab-bar)
         (length> (tab-bar-tabs) 1))
    (tab-close))
   ((length> (frame-list) 1)
    (delete-frame))
   (t
    (user-error "Nothing to delete."))))

;;;###autoload
(defun cw-simple-kill-ring-save-dwim (&optional beg end)
  "Copy the current region or line. When the region is active, use
`kill-ring-save' between the BEG and END positions. Otherwise, copy the
current line."
  (interactive
   (when (region-active-p)
     (list
      (region-beginning)
      (region-end))))
  (if (and beg end)
      (copy-region-as-kill beg end)
    (copy-region-as-kill (line-beginning-position) (line-end-position))
    (setq this-command 'kill-ring-save)))

;;;###autoload
(defun cw-simple-kill-region-dwim (&optional beg end)
  "Do `kill-region' when the region is active, else `kill-ring-save' symbol
at point."
  (interactive
   (when (region-active-p)
     (list
      (region-beginning)
      (region-end))))
  (if (and beg end)
      (kill-region beg end)
    (cw-simple-mark-sexp)
    (copy-region-as-kill (region-beginning) (region-end)))
  (setq this-command 'kill-ring-save))

(defun cw-simple--duplicate-buffer-substring (boundaries)
  "Duplicate buffer substring between BOUNDARIES.
BOUNDARIES is a cons cell representing buffer positions."
  (unless (consp boundaries)
    (error "`%s' is not a cons cell" boundaries))
  (let ((beg (car boundaries))
        (end (cdr boundaries)))
    (goto-char end)
    (newline)
    (insert (buffer-substring-no-properties beg end))))

;;;###autoload
(defun cw-simple-duplicate-line-or-region ()
  "Duplicate the current line or active region."
  (interactive)
  (unless mark-ring                  ; needed when entering a new buffer
    (push-mark (point) t nil))
  (cw-simple--duplicate-buffer-substring
   (if (region-active-p)
       (cons (region-beginning) (region-end))
     (cons (line-beginning-position) (line-end-position))))
  (setq this-command 'yank))

;;;###autoload
(defun cw-simple-yank-replace-line-or-region ()
  "Replace line or region with latest kill.
This command can then be followed by the standard
`yank-pop' (default is bound to \\[yank-pop])."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (line-beginning-position) (line-end-position)))
  (yank)
  (setq this-command 'yank))

;;;###autoload
(defun cw-simple-insert-date (&optional arg)
  "Insert the current date as `cw-simple-date-specifier'.

With optional prefix ARG (\\[universal-argument]) also append the
current time understood as `cw-simple-time-specifier'.

When region is active, delete the highlighted text and replace it
with the specified date."
  (interactive "P")
  (let* ((date cw-simple-date-specifier)
         (time cw-simple-time-specifier)
         (format (if arg (format "%s %s" date time) date)))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (insert (format-time-string format))))

;;;###autoload
(defun cw-simple-other-window ()
  "Wrapper for `other-window' and `next-multiframe-window'.
If there is only one window and multiple frames, call
`next-multiframe-window'. Otherwise, call `other-window'."
  (interactive)
  (if (and (one-window-p) (length> (frame-list) 1))
      (progn
        (call-interactively #'next-multiframe-window)
        (setq this-command #'next-multiframe-window))
    (call-interactively #'other-window)
    (setq this-command #'other-window)))

;;;###autoload
(defun cw-simple-kill-buffer (buffer)
  "Kill current BUFFER without confirmation.
When called interactively, prompt for BUFFER."
  (interactive (list (read-buffer "Select buffer: ")))
  (let ((kill-buffer-query-functions nil))
    (kill-buffer (or buffer (current-buffer)))))

;;;###autoload
(defun cw-simple-window-small-p ()
  "Return non-nil if window is small.
Check if the `window-width' or `window-height' is less than
`split-width-threshold' and `split-height-threshold' respectively."
  (or (and (numberp split-width-threshold)
           (< (window-total-width) split-width-threshold))
      (and (numberp split-height-threshold)
           (< (window-total-height) split-height-threshold))))

;;;###autoload
(defun cw-simple-kill-buffer-current (&optional arg)
  "Kill current buffer.
With optional prefix ARG (\\[universal-argument]) delete the
buffer's window as well. Kill the window regardless of ARG if it
satisfies `cw-simple-window-small-p' and it has no previous
buffers in its history."
  (interactive "P")
  (let ((kill-buffer-query-functions nil))
    (if (or (and (cw-simple-window-small-p)
                 (null (window-prev-buffers)))
            (and arg (not (one-window-p))))
        (kill-buffer-and-window)
      (kill-buffer))))

(defvar cw-simple-override-mode-map (make-sparse-keymap)
  "Keymap of `cw-simple-override-mode'.
Enable that mode to have its key bindings take effect over those of the major mode.")

(define-minor-mode cw-simple-override-mode
  "Enable the `cw-simple-override-mode'."
  :init-value nil
  :global t
  :keymap cw-simple-override-mode-map)

(provide 'cw-simple)
