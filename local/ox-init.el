(require 'ox)
(require 'ox-beamer)

(setq org-directory "~/Dropbox/org"
      +org-attach-dir ".attach/"
      org-attach-directory (expand-file-name +org-attach-dir org-directory))
(setq org-highlight-latex-and-related '(latex)
      org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"))
      org-latex-packages-alist (quote (("" "color" t)
                                       ("" "minted" t)
                                       ("" "parskip" t)
                                       ("" "tikz" t)))
      org-latex-pdf-process '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))
(push (cons "attach" (abbreviate-file-name org-attach-directory)) org-link-abbrev-alist)
  (org-link-set-parameters
   "attach"
   :follow (lambda (link) (find-file (expand-file-name link org-attach-directory)))
   :complete (lambda (&optional _arg)
               (+org--relpath (+org-link-read-file "attach" org-attach-directory)
                              org-attach-directory))
   :face (lambda (link)
           (if (file-exists-p (expand-file-name link org-attach-directory))
               'org-link
             'error)))

