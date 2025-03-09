;;; init-llm.el

(use-package aidermacs
  :if (executable-find "aider")
  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :custom
  (aidermacs-backend 'vterm)
  (aidermacs-auto-commits nil)
  (aidermacs-use-architect-mode nil)
  (aidermacs-default-model "openrouter/anthropic/claude-3.5-sonnet")
  :config
  (add-to-list 'display-buffer-alist
               `("\\*aidermacs.*\\*"
                 (display-buffer-pop-up-window)))
  :bind
  (("C-c j" . aidermacs-transient-menu)))

(setenv "OPENROUTER_API_KEY" (with-temp-buffer
                               (insert-file-contents "~/.config/openrouter/key.txt")
                               (string-trim (buffer-string))))

(provide 'init-llm)
