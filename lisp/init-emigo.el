;; -*- lexical-binding: t -*-

(use-package emigo
 :straight (:host github :repo "MatthewZMD/emigo"
                  :files ("*.el" "*.py" "queries")
                  :build (:not compile))
 :config
 (emigo-enable)
 :custom
 (emigo-model "openrouter/google/gemini-2.5-pro-exp-03-25:free")
 (emigo-base-url "https://openrouter.ai/api/v1")
 (emigo-api-key (with-temp-buffer
                  (insert-file-contents "~/.config/openrouter/key.txt")
                  (string-trim (buffer-string))))
 )

(provide 'init-emigo)
