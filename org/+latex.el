;;; private/org/+latex.el -*- lexical-binding: t; -*-
(add-hook 'org-load-hook #'+org|init-latex)

(def-package! cdlatex
  :commands (turn-on-org-cdlatex))

(defun +org|init-latex ()
  (setq-default org-format-latex-options `(:background ,(doom-color 'bg)
                                                       :foreground ,(doom-color 'fg)
                                                       :scale 1
                                                       :html-foreground ,(doom-color 'fg)
                                                       :html-background "Transparent"
                                                       :html-scale 1.0
                                                       :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
                org-highlight-latex-and-related '(latex)
                org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"))
                org-latex-packages-alist (quote (("" "color" t)
                                                 ("" "minted" t)
                                                 ("" "parskip" t)
                                                 ("" "tikz" t)))
                org-latex-pdf-process '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
                                        "bibtex %b"
                                        "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
                                        "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f")
                org-preview-latex-default-process 'imagemagick
                org-preview-latex-image-directory (concat doom-cache-dir "org-latex/")
                )
  (defun org-latex-clear-fragment ()
    (interactive)
    (delete-directory org-preview-latex-image-directory t))
  )

