;; init-custom.el -*- lexical-binding: t -*-

(defgroup curtain nil
  "Curtain Emacs Configuration Group."
  :group 'convenience)

(defcustom curtain-full-name user-full-name
  "Set user full name within Emacs."
  :group 'curtain
  :type 'string)

;; TODO: make this being a list of email address.
(defcustom curtain-email-address "curtainwk@gmail.com"
  "Set user email address within Emacs."
  :group 'curtain
  :type 'string)

(defcustom curtain-proxy "127.0.0.1:7890"
  "Emacs proxy services."
  :group 'curtain
  :type 'string)

(defcustom curtain-org-directory (expand-file-name "~/Dropbox/org")
  "Set org directory."
  :group 'curtain
  :type 'string)

(defcustom curtain-blog-directory (expand-file-name "~/Documents/site/blog/content-org")
  "Set org blog directory."
  :group 'curtain
  :type 'string)

(defcustom curtain-server-p nil
  "Enable `server-mode' or not."
  :group 'curtain
  :type 'boolean)

(defcustom curtain-icons-p t
  "Display icons or not. Which need install packages to support it."
  :group 'curtain
  :type 'boolean)

(defcustom curtain-package-archives-alist
  (let ((proto (if (gnutls-available-p) "https" "http")))
    `((melpa   . (("gnu"    . ,(format "%s://elpa.gnu.org/packages/" proto))
		  ("nongnu" . ,(format "%s://elpa.nongnu.org/packages/" proto))
		  ("melpa"  . ,(format "%s://melpa.org/packages/" proto))))
      (bfsu    . (("gnu"    . ,(format "%s://mirrors.bfsu.edu.cn/elpa/gnu/" proto))
		  ("nongnu" . ,(format "%s://mirrors.bfsu.edu.cn/elpa/nongnu/" proto))
		  ("melpa"  . ,(format "%s://mirrors.bfsu.edu.cn/elpa/melpa/" proto))))
      (iscas   . (("gnu"    . ,(format "%s://mirrors.iscas.ac.cn/elpa/gnu/" proto))
		  ("nongnu" . ,(format "%s://mirrors.iscas.ac.cn/elpa/nongnu/" proto))
		  ("melpa"  . ,(format "%s://mirrors.iscas.ac.cn/elpa/melpa/" proto))))
      (netease . (("gnu"    . ,(format "%s://mirrors.163.com/elpa/gnu/" proto))
		  ("nongnu" . ,(format "%s://mirrors.163.com/elpa/nongnu/" proto))
		  ("melpa"  . ,(format "%s://mirrors.163.com/elpa/melpa/" proto))))
      (sjtu    . (("gnu"    . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/" proto))
		  ("nongnu" . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu/" proto))
		  ("melpa"  . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/" proto))))
      (tuna    . (("gnu"    . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/" proto))
		  ("nongnu" . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/" proto))
		  ("melpa"  . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/" proto))))
      (ustc    . (("gnu"    . ,(format "%s://mirrors.ustc.edu.cn/elpa/gnu/" proto))
		  ("nongnu" . ,(format "%s://mirrors.ustc.edu.cn/elpa/nongnu/" proto))
		  ("melpa"  . ,(format "%s://mirrors.ustc.edu.cn/elpa/melpa/" proto))))))
  "List of the package archives."
  :group 'curtain
  :type '(alist :key-type (symbol :tag "Archive group name")
		:value-type (alist :key-value (string :tag "Archive name")
				   :value-type (string :tag "URL name"))))

(defcustom curtain-package-archives 'bfsu
  "Set package archives from which to fetch."
  :group 'curtain
  :set (lambda (symbol value)
	 (set symbol value)
	 (setq package-archives
	       (or (alist-get value curtain-package-archives-alist)
		   (error "Unknown package archives: `%s'" value))))
  :type `(choice ,@(mapcar
		    (lambda (item)
		      (let ((name (car item)))
			(list 'const
			      :tag (capitalize (symbol-name name))
			      name)))
		    curtain-package-archives-alist)))

(defcustom curtain-theme-alist
  '((default . doom-one)
    (pro     . doom-monokai-pro))
  "List of themes mapped to internal themes."
  :group 'curtain
  :type '(alist :key-type (symbol :tag "theme")
		:value-type (symbol :tag "internal theme")))

(defcustom curtain-theme 'default
  "The theme used."
  :group 'curtain
  :type `(chioce ,@(mapcar
		    (lambda (item)
		      (let ((name (car item)))
			(list 'const
			      :tag (capitalize (symbol-name name))
			      name)))
		    curtain-theme-alist)
		 symbol))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-custom)
