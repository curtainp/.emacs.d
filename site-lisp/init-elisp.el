;; -*- lexical-binding: t -*-


(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
	 ([remap describe-command]  . helpful-command)
	 ([remap describe-variable] . helpful-variable)
	 ([remap describe-key]      . helpful-key)
	 ([remap describe-symbol]   . helpful-symbol)
	 :map emacs-lisp-mode-map
	 ("C-c C-d"                 . helpful-at-point)
	 :map lisp-interaction-mode-map
	 ("C-c C-d"                 . helpful-at-point)
	 :map helpful-mode-map
	 ("r"                       . remove-hook-at-point))
  :hook (helpful-mode . cursor-sensor-mode)
  :init
  (with-no-warnings
    (with-eval-after-load 'apropos
      ;; patch apropos buttons to call helpful instead of help
      (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
        (button-type-put
         fun-bt 'action
         (lambda (button)
           (helpful-callable (button-get button 'apropos-symbol)))))
      (dolist (var-bt '(apropos-variable apropos-user-option))
        (button-type-put
         var-bt 'action
         (lambda (button)
           (helpful-variable (button-get button 'apropos-symbol)))))))
  :config
  (with-no-warnings
    ;; Open the buffer in other window
    (defun my-helpful--navigate (button)
      "Navigate to the path this BUTTON represents."
      (find-file-other-window (substring-no-properties (button-get button 'path)))
      ;; We use `get-text-property' to work around an Emacs 25 bug:
      ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=f7c4bad17d83297ee9a1b57552b1944020f23aea
      (-when-let (pos (get-text-property button 'position
                                         (marker-buffer button)))
        (helpful--goto-char-widen pos)))
    (advice-add #'helpful--navigate :override #'my-helpful--navigate)))
  
(provide 'init-elisp)
