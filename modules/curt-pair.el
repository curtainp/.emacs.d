;;; curt-pair.el -*- lexical-binding: t -*-

(defgroup curt-pair nil
  "Insert character pair around symbol or region."
  :group 'editing)

(defcustom curt-pair-pairs
  '((?'   :description "Single quotes"            :pair (?' . ?'))
    (?\"  :description "Double quotes"            :pair (?\" . ?\"))
    (?‘   :description "Single curly quotes"      :pair (?‘ . ?’))
    (?“   :description "Double curly quotes"      :pair (?“ . ?”))
    (?\>  :description "Natural language quotes"  :pair curt-pair-insert-natural-language-quotes)
    (?\(  :description "Parentheses"              :pair (?\( . ?\)))
    (?{   :description "Curly brackets"           :pair (?{ . ?}))
    (?\[  :description "Square brackets"          :pair (?\[ . ?\]))
    (?\<  :description "Angled brackets"          :pair (?\< . ?\>))
    (?@   :description "At sign"                  :pair (?@ . ?@))
    (?=   :description "Equals sign"              :pair (?= . ?=))
    (?+   :description "Plus sign"                :pair (?+ . ?+))
    (?`   :description "Backticks"                :pair curt-pair-insert-backticks)
    (?~   :description "Tildes"                   :pair (?~ . ?~))
    (?*   :description "Asterisks"                :pair (?* . ?*))
    (?/   :description "Forward slashes"          :pair (?/ . ?/))
    (?_   :description "Underscores"              :pair (?_ . ?_)))
  "Alist of pairs for use with `curt-pair-insert'.
Each element in the list is a list whose `car' is a character and the `cdr' is a plist
with a `:description' and `:pair' keys. the `:description' is a string used to describe
the character/pair in interactive use, while `:pair' is a cons cell referencing the
opening and closing characters.

The value of `:pair' can also be the unquoted symbol of a funtion. the function is called
with no arguments and must return a cons cell of two character. example of such functions
are `curt-pair-insert-natural-language-quotes'."
  :type '(alist
          :key-type character
          :value-type (plist :options (((const :tag "Pair description" :description) string)
                                       ((const :tag "Characters" :pair)
                                        (choice (cons character character) function)))))
  :group 'curt-pair)

(defun curt-pair-insert-single-quotes ()
  "Return pair of single quotes for `curt-pair-pairs'.
When the major mode is derived from `rust-mode', return one single quote only, else two."
  (if (derived-mode-p 'rust-mode 'rust-ts-mode)
      (cons ?' nil)
    (cons ?' ?')))

(defun curt-pair-insert-backticks ()
  "Return pair of backticks for `curt-pair-pairs'.
When the major mode is derived from `lisp-mode', return a pair of
backtick and single quote, else two backticks."
  (if (derived-mode-p 'lisp-mode 'lisp-data-mode)
      (cons ?` ?')
    (cons ?` ?`)))

(defun curt-pair-insert-natural-language-quotes ()
  "Return pair of quotes for `curt-pair-pairs', per natural language."
  (cond
   ((and current-input-mode
         (string-match-p "\\(rime\\)" current-input-method)) ;; I use rime as Chinese input method
    (cons ?« ?»))
   (t (cons ?\" ?\"))))

(defvar curt-pair--insert-history nil
  "Minibuffer history of `curt-pair--insert-prompt'.")

(defun curt-pair--annotate (character)
  "Annotate CHARACTER with its description in `curt-pair-pairs'."
  (when-let* ((char (if (characterp character) character (string-to-char character)))
              (plist (alist-get char curt-pair-pairs))
              (description (plist-get plist :description)))
    (format "  %s" description)))

(defun curt-pair--get-pair (character)
  "Get the pair of corresponding to CHARACTER."
  (when-let* ((char (if (characterp character) character (string-to-char character)))
              (plist (alist-get char curt-pair-pairs))
              (pair (plist-get plist :pair)))
    pair))

(defun curt-pair--insert-prompt ()
  "Prompt for pair among `curt-pair-pairs'."
  (let ((default (car curt-pair--insert-history))
        (candidates (mapcar (lambda (char) (char-to-string (car char))) curt-pair-pairs))
        (completion-extra-properties `(:annotation-function ,#'curt-pair--annotate)))
    (completing-read
     (format-prompt "Select pair" default)
     candidates nil :require-match
     nil 'curt-pair--insert-history default)))

(defun curt-pair--insert-bounds ()
  "Return boundaries of symbol at point or active region."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (bounds-of-thing-at-point 'symbol)))

;;;###autoload
(defun curt-pair-insert (pair n)
  "Insert N number of pair around object at point.
PAIR is one among `curt-pair-pairs'. the object at point is either a symbol or the
boundaries of the active region. N is a numeric prefix argument, defaulting to 1 if
none is provided in interactive use."
  (interactive
   (list
    (curt-pair--get-pair (curt-pair--insert-prompt))
    (prefix-numeric-value current-prefix-arg)))
  (let* ((bounds (curt-pair--insert-bounds))
         (beg (car bounds))
         (end (1+ (cdr bounds)))
         (characters (if (functionp pair) (funcall pair) pair)))
    (dotimes (_ n)
      (save-excursion
        (goto-char beg)
        (insert (car characters))
        (goto-char end)
        (setq end (1+ end))
        (insert (cdr characters))))
    (goto-char (+ end (1- n)))))

;;;###autoload
(defun curt-pair-delete ()
  "Delete pair following or preceding point.
For Emacs version 28 or higher, the feedback's delay is controlled by
`delete-pair-blink-delay'."
  (interactive)
  (if (eq (point) (cdr (bounds-of-thing-at-point 'sexp)))
      (delete-pair -1)
    (delete-pair 1)))

(provide 'curt-pair)
