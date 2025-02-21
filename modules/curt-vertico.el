;;; -*- lexical-binding: t -*-

(require 'vertico)

(defvar curt-vertico-multiform-minimal
  '(unobtrusive
    (vertico-flat-format . ( :multiple  ""
                             :single    ""
                             :prompt    ""
                             :separator ""
                             :ellipsis  ""
                             :no-match  ""))
    (vertico-preselect . prompt))
  "List of configurations for minimal Vertico multiform.
The minimal view is intended to be more private or less
revealing.  This is important when, for example, a prompt shows
names of people.  Of course, such a view also provides a minimal
style for general usage.

Toggle the vertical view with the `vertico-multiform-vertical'
command or use the commands `curt-vertico-private-next' and
`curt-vertico-private-previous', which toggle the vertical view
automatically.")

(defvar curt-vertico-multiform-maximal
  '((vertico-count . 10)
    (vertico-preselect . directory)
    (vertico-resize . t))
  "List of configurations for maximal Vertico multiform.")

(defun curt-vertico--match-directory (str)
  "Match directory delimiter in STR."
  (string-suffix-p "/" str))

;; From the Vertico documentation.
(defun curt-vertico-sort-directories-first (files)
  "Sort directories before FILES."
  (setq files (vertico-sort-alpha files))
  (nconc (seq-filter #'curt-vertico--match-directory files)
         (seq-remove #'curt-vertico--match-directory files)))

(defun curt-vertico-private-next ()
  "Like `vertico-next' but toggle vertical view if needed.
This is done to accommodate `curt-vertico-multiform-minimal'."
  (interactive)
  (if vertico-unobtrusive-mode
      (progn
        (vertico-multiform-vertical)
        (vertico-next 1))
    (vertico-next 1)))

(defun curt-vertico-private-previous ()
  "Like `vertico-previous' but toggle vertical view if needed.
This is done to accommodate `curt-vertico-multiform-minimal'."
  (interactive)
  (if vertico-unobtrusive-mode
      (progn
        (vertico-multiform-vertical)
        (vertico-previous 1))
    (vertico-previous 1)))

(defun curt-vertico-private-complete ()
  "Expand contents and show remaining candidates, if needed.
This is done to accommodate `curt-vertico-multiform-minimal'."
  (interactive)
  (if (and vertico-unobtrusive-mode (> vertico--total 1))
      (progn
        (minibuffer-complete)
        (curt-vertico-private-next))
    (vertico-insert)))

(defun curt-vertico-private-exit ()
  "Exit with the candidate if `curt-vertico-multiform-minimal'.
If there are more candidates that match the given input, expand the
minibuffer to show the remaining candidates and select the first one.
Else do `vertico-exit'."
  (interactive)
  (cond
   ((= vertico--total 1)
    (minibuffer-complete)
    (vertico-exit))
   ((and vertico-unobtrusive-mode
         (not minibuffer--require-match)
         (or (string-empty-p (minibuffer-contents))
             minibuffer-default
             (eq vertico-preselect 'directory)
             (eq vertico-preselect 'prompt)))
    (vertico-exit-input))
   ((and vertico-unobtrusive-mode (> vertico--total 1))
    (minibuffer-complete-and-exit)
    (curt-vertico-private-next))
   (t
    (vertico-exit))))

(provide 'curt-vertico)
