;;; -*- lexical-binding: t -*-

(use-package ox-latex
  :straight nil
  :after org
  :config
  (setq
   org-entities-user '(("ws" "\\ " nil " " " " " " " "))
   org-latex-prefer-user-labels t
   org-startup-with-latex-preview nil
   org-preview-latex-default-process 'dvisvgm
   org-preview-latex-process-alist'((dvisvgm :programs
                                     ("xelatex" "dvisvgm")
                                     :description "xdv > svg"
                                     :message "you need to install the programs: xelatex and dvisvgm."
                                     :use-xcolor t
                                     :image-input-type "xdv"
                                     :image-output-type "svg"
                                     :image-size-adjust (1.7 . 1.5)
                                     :latex-compiler
                                     ("xelatex -no-pdf -interaction nonstopmode -shell-escape -output-directory %o %f")
                                     :image-converter
                                     ("dvisvgm %f -e -n -b min -c %S -o %O"))
                                    (imagemagick :programs
                                     ("xelatex" "convert")
                                     :description "pdf > png"
                                     :message "you need to install the programs: xelatex and imagemagick."
                                     :use-xcolor t
                                     :image-input-type "pdf"
                                     :image-output-type "png"
                                     :image-size-adjust (1.0 . 1.0)
                                     :latex-compiler
                                     ("xelatex -interaction nonstopmode -output-directory %o %f")
                                     :image-converter
                                     ("convert -density %D -trim -antialias %f -quality 100 %O")))
   org-latex-hyperref-template "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c}, \n pdflang={%L},\n colorlinks=true,\n linkcolor=black}\n"
   org-format-latex-options '(:foreground default :background "Transparent" :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                              ("begin" "$1" "$" "$$" "\\(" "\\["))
   org-latex-src-block-backend 'minted
   org-latex-minted-options '(("breaklines")
                              ("bgcolor" "bg"))
   org-latex-compiler "xelatex"
   org-latex-packages-alist '(("" "amsthm")
                              ("" "amsfonts")
                              ("" "bm")
                              ("" "tikz")
                              ("" "xcolor" t)
                              ("cache=false" "minted" t))
   org-latex-pdf-process '("latexmk -f -xelatex -shell-escape -output-directory=%o %F")
   org-latex-classes '(("Notes" "\\documentclass{ctexart}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n\\usepackage{/home/eli/.emacs.d/private/NotesTeXV3}"
                        ("\\part{%s}" . "\\part*{%s}")
                        ("\\section{%s}" . "\\section*{%s}")
                        ("\\subsection{%s}" . "\\subsection*{%s}")
                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                       ("article_cn" "\\documentclass[11pt]{ctexart}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n[EXTRA]\n\\definecolor{bg}{rgb}{0.95,0.95,0.95}"
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

(use-package latex
  :straight auctex)


(use-package cdlatex
  :straight t
  :commands (org-cdlatex-mode turn-on-org-cdlatex)
  :hook (LaTeX-mode . turn-on-org-cdlatex)
  :hook (org-mode . org-cdlatex-mode)
  :bind (:map cdlatex-mode-map
              ("<tab>" . cdlatex-tab)))


(provide 'init-latex)
