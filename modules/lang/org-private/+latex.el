;;; lang/org-private/+latex.el -*- lexical-binding: t; -*-
(add-hook 'org-load-hook #'+org-private|init-latex t)

(def-package! cdlatex
  :commands (org-cdlatex-mode
             cdlatex-mode
             turn-on-cdlatex
             turn-on-org-cdlatex)
  :init
  (setq cdlatex-math-modify-alist '((?B "\\mathbb" nil t nil nil))))


(def-package! webkit-katex-render :load-path "~/Source/playground/emacs-webkit-katex-render"
  :commands (webkit-katex-render-mode)
  :config
  (setq webkit-katex-render--background-color (doom-color 'bg)))

(defun +org-private|init-latex ()
  (setq-default org-latex-prefer-user-labels t)
  (cond
   ((eq window-system 'mac)
    (setq-default org-format-latex-options
                  `(:background ,(doom-color 'bg)
                                :foreground ,(doom-color 'fg)
                                :scale 1
                                :html-foreground ,(doom-color 'fg)
                                :html-background "Transparent"
                                :html-scale 1.0
                                :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
                  org-highlight-latex-and-related '(latex)
                  org-latex-packages-alist
                  '(("" "color" t)
                    ("" "minted" t)
                    ("" "parskip" t)
                    ("" "koma-script" t)
                    ("" "tikz" t))
                  org-latex-pdf-process '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f")
                  org-preview-latex-default-process 'dvisvgm
                  org-preview-latex-image-directory (concat doom-cache-dir "org-latex/")))
   ((eq window-system 'ns)
    (add-to-list 'org-latex-classes
             '("scrreprt"
               "\\documentclass{scrreprt}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (setq-default org-format-latex-options
                  `(:background ,(doom-color 'bg)
                                :foreground ,(doom-color 'fg)
                                :scale 2.0
                                :html-foreground ,(doom-color 'fg)
                                :html-background "Transparent"
                                :html-scale 1.0
                                :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
                  org-highlight-latex-and-related '(latex)
                  org-latex-listings 'minted
                  org-latex-packages-alist
                  '(("" "color" t)
                    ("" "minted" t)
                    ("" "booktabs" t)
                    ("" "tabularx" t)
                    ("" "parskip" t)
                    ("" "tikz" t))
                  org-latex-pdf-process '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f")
                  org-preview-latex-default-process 'imagemagick
                  org-preview-latex-image-directory (concat doom-cache-dir "org-latex/")))))
