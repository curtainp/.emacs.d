;;; curt-simple.el -*- lexical-binding: t -*-

;; copy from Protesilaos Stavrou

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))


(defgroup curt-simple ()
  "Generic utilities for improve built-in simple.el"
  :group 'editing)

(defcustom curt-simple-date-specifier "%F"
  "Date specifier for `format-time-string'.
Used by `curt-simple-insert-date'. '%F' is the ISO 8601 date format (like %-4Y-%m-%d)"
  :type 'string
  :group 'curt-simple)

(defcustom curt-simple-time-specifier "%R %z"
  "Time specifier for `format-time-string'.
Used by `curt-simple-insert-date'. '%R %z' is like %H:%M, %z is the numeric form."
  :type 'string
  :group 'curt-simple)

;;;###autoload
(defun curt-simple-keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

;;;###autoload
(defun curt-simple-insert-date (&optional arg)
  "Insert the current date as `curt-simple-date-specifier'.

With optional prefix ARG (\\[universal-argument]) also append the
current time understood as `curt-simple-time-specifier'.

When region is active, delete the highlighted text and replace it
with the specified date."
  (interactive "P")
  (let* ((date curt-simple-date-specifier)
         (time curt-simple-time-specifier)
         (format (if arg (format "%s %s" date time) date)))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (insert (format-time-string format))))

;;;###autoload
(defun curt-simple-other-window ()
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
(defun curt-simple-kill-buffer (buffer)
  "Kill current BUFFER without confirmation.
When called interactively, prompt for BUFFER."
  (interactive (list (read-buffer "Select buffer: ")))
  (let ((kill-buffer-query-functions nil))
    (kill-buffer (or buffer (current-buffer)))))

;;;###autoload
(defun curt-simple-window-small-p ()
  "Return non-nil if window is small.
Check if the `window-width' or `window-height' is less than
`split-width-threshold' and `split-height-threshold' respectively."
  (or (and (numberp split-width-threshold)
           (< (window-total-width) split-width-threshold))
      (and (numberp split-height-threshold)
           (< (window-total-height) split-height-threshold))))

;;;###autoload
(defun curt-simple-kill-buffer-current (&optional arg)
  "Kill current buffer.
With optional prefix ARG (\\[universal-argument]) delete the
buffer's window as well. Kill the window regardless of ARG if it
satisfies `curt-simple-window-small-p' and it has no previous
buffers in its history."
  (interactive "P")
  (let ((kill-buffer-query-functions nil))
    (if (or (and (curt-simple-window-small-p)
                 (null (window-prev-buffers)))
            (and arg (not (one-window-p))))
        (kill-buffer-and-window)
      (kill-buffer))))

(defvar curt-simple-override-mode-map (make-sparse-keymap)
  "Keymap of `curt-simple-override-mode'.
Enable that mode to have its key bindings take effect over those of the major mode.")

(define-minor-mode curt-simple-override-mode
  "Enable the `curt-simple-override-mode'."
  :init-value nil
  :global t
  :keymap curt-simple-override-mode-map)

(provide 'curt-simple)
