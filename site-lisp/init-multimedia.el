;; -*- lexical-binding: t; -*-

(use-package sort-tab
  :straight '(sort-tab :type git :host github :repo "manateelazycat/sort-tab")
  :bind (:map global-map
	 ("M-0" . sort-tab-select-visible-tab)
	 ("M-1" . sort-tab-select-visible-tab)
	 ("M-2" . sort-tab-select-visible-tab)
	 ("M-3" . sort-tab-select-visible-tab)
	 ("M-4" . sort-tab-select-visible-tab)
	 ("M-5" . sort-tab-select-visible-tab)
	 ("M-6" . sort-tab-select-visible-tab)
	 ("M-7" . sort-tab-select-visible-tab)
	 ("M-8" . sort-tab-select-visible-tab)
	 ("M-9" . sort-tab-select-visible-tab)
	 ("C-;" . sort-tab-close-current-tab))
  :init
  (sort-tab-mode))

(use-package holo-layer
  :disabled t
  :straight '(holo-layer :type git :host github :repo "manateelazycat/holo-layer"
			 :files (:defaults "*.el" "*.py" "icon_cache" "plugin" "resources")
			 :build (:not compile))
  :demand t
  :custom
  (holo-layer-sort-tab-ui t)
  :config
  (holo-layer-enable))

(provide 'init-multimedia)
