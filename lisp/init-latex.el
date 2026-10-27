;;; -*- lexical-binding: t -*-

;; 默认公式预览：latex（pdfTeX 的 DVI 模式）-> dvisvgm -> SVG。
;; 预编译需要 preview.sty 和 mylatexformat.ltx；Emacs 需要 SVG 图像支持。
;; PDF 导出使用 pdflatex + latexmk；缺少 latexmk 时回退为三次编译。
;; AUCTeX、CDLaTeX 由下方 straight 声明安装；新版预览随 Org 开发版提供。
;;
;; Arch Linux：安装本配置英文预览和 PDF 导出所需的 TeX 集合：
;;   sudo pacman -S --needed texlive-latex texlive-latexrecommended \
;;     texlive-latexextra texlive-fontsrecommended texlive-pictures \
;;     texlive-binextra dvisvgm
;;   texlive-latex：amsmath、amsthm、amsfonts、bm 等基础数学宏包。
;;   texlive-latexrecommended：常用导出宏包（如 hyperref、xcolor）。
;;   texlive-latexextra：preview、mylatexformat、minted / latexminted。
;;   texlive-fontsrecommended：Latin Modern 等字体，也供 XeLaTeX 使用。
;;   texlive-pictures：PGF/TikZ；texlive-binextra：latexmk。
;; 可选：Notes / article_cn 中文文档及 XeLaTeX：
;;   sudo pacman -S --needed texlive-xetex texlive-langchinese
;; Arch 的 TeX 文件使用 pacman 管理，不使用 sudo tlmgr 修改这些文件。
;;
;; macOS：推荐完整的无 GUI MacTeX，包含上述 TeX 宏包、字体和编译工具：
;;   brew install --cask mactex-no-gui
;; 已安装完整 MacTeX 时无需重复安装；不要与 BasicTeX 同时安装。
;; 安装后重开终端和 Emacs，并确保 /Library/TeX/texbin 同时出现在
;; Emacs 的 exec-path 和 PATH 中（从 Dock 启动时尤其需要检查）。
;;
;; minted：当前 TeX Live 的 minted 3 自带 latexminted 及 Pygments 组件；
;; 使用发行版配套版本时无需另外 pip install。旧 minted 2 才依赖 pygmentize
;; （Arch: python-pygments；macOS: brew install pygments）。
;;
;; 安装检查（终端）：
;;   latex --version; pdflatex --version; dvisvgm --version; latexmk -v
;;   kpsewhich preview.sty; kpsewhich mylatexformat.ltx
;;   kpsewhich minted.sty; latexminted --version
;;   kpsewhich lmroman10-regular.otf
;; 中文文档另检查：xelatex --version; kpsewhich ctexart.cls
;; Emacs 内检查：(executable-find "latex")、(executable-find "dvisvgm")、
;; (executable-find "latexmk") 和 (image-type-available-p 'svg)。
;;
;; 包信息与安装说明：
;; https://archlinux.org/packages/extra/any/texlive-latexextra/
;; https://archlinux.org/packages/extra/any/texlive-binextra/
;; https://formulae.brew.sh/cask/mactex-no-gui
;; https://tug.ctan.org/macros/latex/contrib/minted/minted.pdf

(use-package ox-latex
  :straight nil
  :after org
  :config
  (setq
   org-entities-user '(("ws" "\\ " nil " " " " " " " "))
   org-latex-prefer-user-labels t
   ;; Preview all formulas on opening an Org buffer.  Per-file opt-out:
   ;; #+STARTUP: nolatexpreview
   org-startup-with-latex-preview t
   org-latex-hyperref-template "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c}, \n pdflang={%L},\n colorlinks=true,\n linkcolor=black}\n"
   org-latex-src-block-backend 'minted
   org-latex-minted-options '(("breaklines"))
   org-latex-compiler "pdflatex"
   org-latex-packages-alist '(("" "amsthm" t)
                              ("" "amsfonts" t)
                              ("" "bm" t)
                              ("" "tikz" nil)
                              ("" "xcolor" t)
                              ("cache=false" "minted" nil))
   org-latex-pdf-process
   (if (executable-find "latexmk")
       '("latexmk -pdf -%latex -interaction=nonstopmode -shell-escape -output-directory=%o %f")
     ;; Resolve ordinary references without requiring latexmk.
     ;; Bibliographies and indexes still need their own build tools.
     (make-list 3 "%latex -interaction=nonstopmode -halt-on-error -shell-escape -output-directory=%o %f"))
   org-latex-classes '(("Notes" "\\documentclass{ctexart}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n[EXTRA]"
                        ("\\part{%s}" . "\\part*{%s}")
                        ("\\section{%s}" . "\\section*{%s}")
                        ("\\subsection{%s}" . "\\subsection*{%s}")
                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                       ("article_cn" "\\documentclass[11pt]{ctexart}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n[EXTRA]"
                        ("\\section{%s}" . "\\section*{%s}")
                        ("\\subsection{%s}" . "\\subsection*{%s}")
                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                       ("beamer" "\\documentclass[ignorenonframetext,presentation]{beamer}"
                        ("\\section{%s}" . "\\section*{%s}")
                        ("\\subsection{%s}" . "\\subsection*{%s}"))
                       ("article" "\\documentclass[11pt]{article}"
                        ("\\section{%s}" . "\\section*{%s}")
                        ("\\subsection{%s}" . "\\subsection*{%s}")
                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                       ("report" "\\documentclass[11pt]{report}"
                        ("\\part{%s}" . "\\part*{%s}")
                        ("\\chapter{%s}" . "\\chapter*{%s}")
                        ("\\section{%s}" . "\\section*{%s}")
                        ("\\subsection{%s}" . "\\subsection*{%s}")
                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                       ("book" "\\documentclass[11pt]{book}"
                        ("\\part{%s}" . "\\part*{%s}")
                        ("\\chapter{%s}" . "\\chapter*{%s}")
                        ("\\section{%s}" . "\\section*{%s}")
                        ("\\subsection{%s}" . "\\subsection*{%s}")
                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
  )

;; Use Org 10's built-in asynchronous preview pipeline.
(use-package org-latex-preview
  :straight nil
  :after org
  :hook (org-mode . org-latex-preview-mode)
  :config
  (setq org-latex-preview-process-default 'dvisvgm
        ;; Precompile the default pdfLaTeX preamble for faster previews.
        ;; Org disables this buffer-locally for XeLaTeX documents.
        org-latex-preview-process-precompile t
        org-latex-preview-mode-display-live '(block edit-special)
        org-latex-preview-mode-update-delay 0.3)
  (plist-put org-latex-preview-appearance-options :zoom 1.5)
  (plist-put org-latex-preview-appearance-options :page-width 0.8))

(use-package latex
  :straight auctex
  :defer t)


(use-package cdlatex
  :straight t
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (org-mode . org-cdlatex-mode)
         (org-mode . my/org-latex-input-setup))
  :bind (:map cdlatex-mode-map
              ("<tab>" . cdlatex-tab)))

(defun my/org-tab-expand-snippet-or-latex ()
  "Handle Org TAB by expanding yas snippets before cdlatex."
  (or (and (bound-and-true-p yas-minor-mode)
           (yas-expand))
      (org-try-cdlatex-tab)))

(defun my/org-latex-input-setup ()
  "Prefer direct yas expansion in Org over ACM's yas completion."
  (add-hook 'org-tab-first-hook #'my/org-tab-expand-snippet-or-latex nil t)
  (when (boundp 'acm-enable-yas)
    (setq-local acm-enable-yas nil)))


(provide 'init-latex)
