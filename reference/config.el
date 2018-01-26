(add-hook 'org-mode-hook #'org-ref-mode)

(def-package! org-ref-ivy :load-path "modules/private/reference/local/org-ref-ivy"
  :commands (org-ref-mode
             org-ref-bibtex-next-entry
             org-ref-bibtex-previous-entry
             org-ref-doi-utils-get-bibtex-entry-pdf
             org-ref-ivy-insert-cite-link
             org-ref-open-in-browser
             org-ref-open-bibtex-notes
             org-ref-open-bibtex-pdf
             org-ref-bibtex-hydra/body
             org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
             org-ref-sort-bibtex-entry
             arxiv-add-bibtex-entry
             arxiv-get-pdf-add-bibtex-entry
             doi-utils-add-bibtex-entry-from-doi
             isbn-to-bibtex
             pubmed-insert-bibtex-from-pmid)
  :config
  (require 'ivy-bibtex)
  (require 'org-ref-pdf)
  (setq bibtex-dialect 'BibTeX
        org-ref-completion-library 'org-ref-ivy-cite
        org-ref-clean-bibtex-entry-hook '(org-ref-bibtex-format-url-if-doi
                                          org-ref-key-comma
                                          org-ref-replace-nonascii
                                          org-ref-&
                                          org-ref-%
                                          org-ref-title-case-article
                                          org-ref-clean-year
                                          my-org-ref-key
                                          org-ref-clean-doi
                                          org-ref-clean-pages
                                          org-ref-check-journal
                                          org-ref-sort-bibtex-entry)
        org-ref-default-bibliography '("~/Dropbox/org/reference/Bibliography.bib")
        org-ref-bibliography-notes "~/Dropbox/org/reference/ref.org"
        org-ref-pdf-directory "~/Dropbox/org/reference/pdf/"
        org-ref-note-title-format "* %t
 :PROPERTIES:
  :CUSTOM_ID: %k
 :END:
")

  )
(def-package! org-ref-doi-utils :load-path "modules/private/reference/local/org-ref-ivy"
  :after org-ref-ivy)
(def-package! org-ref-url-utils :load-path "modules/private/reference/local/org-ref-ivy"
  :after org-ref-ivy)
(def-package! org-ref-arxiv :load-path "modules/private/reference/local/org-ref-ivy"
  :after org-ref-ivy)
(def-package! org-ref-wos :load-path "modules/private/reference/local/org-ref-ivy"
  :after org-ref-ivy)
(def-package! org-ref-scopus :load-path "modules/private/reference/local/org-ref-ivy"
  :after org-ref-ivy)
(def-package! ivy-bibtex
  :commands ivy-bibtex
  :after org
  :config
  (defun bibtex-completion-open-uri (keys)
    "Open the associated URL or DOI in a browser."
    (dolist (key keys)
      (let* ((entry (bibtex-completion-get-entry key))
             (uri (bibtex-completion-get-value "uri" entry))
             (uri (s-replace "\\url{" "" uri))
             (uri (s-replace "}" "" uri))
             )
        (start-process "Open URI" nil "/usr/bin/open" uri)
        )))
  (defun bibtex-completion-quicklook (keys)
    "Open the associated URL or DOI in a browser."
    (dolist (key keys)
      (let* ((entry (bibtex-completion-get-entry key))
             (pdf (car (bibtex-completion-find-pdf entry)))
             )
        (start-process "Live Preview" nil "/usr/bin/qlmanage" "-p" pdf)
        )))
  (ivy-bibtex-ivify-action bibtex-completion-quicklook ivy-bibtex-quicklook)
  (setq
   bibtex-completion-additional-search-fields '("journal")
   bibtex-completion-pdf-symbol "@"
   bibtex-completion-notes-symbol "#"
   bibtex-completion-display-formats '((t . "${author:20} ${journal:10} ${year:4} ${title:*} ${=type=:3} ${=has-pdf=:1}${=has-note=:1}")))
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-key)
  (ivy-add-actions 'ivy-bibtex '(("SPC" ivy-bibtex-quicklook "Quick look")))
  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default))
        bibtex-completion-bibliography "~/Dropbox/org/reference/Bibliography.bib"
        bibtex-completion-library-path "~/Dropbox/org/reference/pdf/"
        bibtex-completion-notes-path "~/Dropbox/org/reference/ref.org"
        bibtex-completion-pdf-field "file"
        bibtex-completion-pdf-open-function (lambda (fpath) (start-process "open" "*open*" "open" fpath))))


