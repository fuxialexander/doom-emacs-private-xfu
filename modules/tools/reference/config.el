;;; tools/reference/config.el -*- lexical-binding: t; -*-

(def-package! org-ref :load-path "~/Source/playground/org-ref"
  :commands (org-ref-bibtex-next-entry
             org-ref-bibtex-previous-entry
             doi-utils-get-bibtex-entry-pdf
             org-ref-ivy-insert-cite-link
             org-ref-find-bibliography
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
  :init
  (defvar org-ref-ivy-cite-actions
    '(
      ("b" ivy-bibtex-show-entry "Open entry")
      ("p" ivy-bibtex-open-pdf "Open pdf")
      ("n" ivy-bibtex-edit-notes "Open notes")
      ("u" ivy-bibtex-open-url-or-doi "Open url or doi")
      ("U" org-ref-ivy-bibtex-get-update-for-entry "Update entry from doi")
      ("P" org-ref-ivy-bibtex-get-pdf-for-entry "Update PDF for entry")
      ("SPC" ivy-bibtex-quicklook "Quick look")
      ("k" org-ref-ivy-set-keywords "Add keywords")
      ("e" org-ref-ivy-bibtex-email-entry "Email entry")
      ("f" org-ref-ivy-bibtex-insert-formatted-citation "Insert formatted citation")
      ("F" org-ref-ivy-bibtex-copy-formatted-citation "Copy formatted citation"))
    "List of additional actions for `org-ref-ivy-insert-cite-link'.
The default action being to insert a citation.")

  (setq org-ref-completion-library 'org-ref-ivy-cite
        doi-utils-pdf-url-functions (list
                                     ;; 'aps-pdf-url
                                     'biorxiv-pdf-url
                                     'science-pdf-url
                                     'nature-pdf-url
                                     'pnas-pdf-url
                                     'oup-pdf-url
                                     'bmc-pdf-url
                                     'wiley-pdf-url
                                     'springer-chapter-pdf-url
                                     'springer-pdf-url
                                     'jstor-pdf-url
                                     'tandfonline-pdf-url
                                     'sage-pdf-url
                                     ;; 'acs-pdf-url-1
                                     ;; 'acs-pdf-url-2
                                     ;; 'iop-pdf-url
                                     ;; 'aip-pdf-url
                                     'science-direct-pdf-url
                                     'linkinghub-elsevier-pdf-url
                                     ;; 'ecs-pdf-url
                                     ;; 'ecst-pdf-url
                                     ;; 'rsc-pdf-url
                                     ;; 'jneurosci-pdf-url
                                     'ieee-pdf-url
                                     'ieee2-pdf-url
                                     'ieee3-pdf-url
                                     'acm-pdf-url
                                     ;; 'osa-pdf-url
                                     'asme-biomechanical-pdf-url
                                     ;; 'generic-full-pdf-url
                                     'generic-as-get-pdf-url))
  :config
  (def-package! ivy-bibtex
    :config
    (ivy-bibtex-ivify-action bibtex-completion-quicklook ivy-bibtex-quicklook)
    (setq
     bibtex-completion-additional-search-fields '("journal")
     bibtex-completion-pdf-symbol "@"
     bibtex-completion-notes-symbol "#"
     bibtex-completion-display-formats '((t . "${=has-pdf=:1}${=has-note=:1} ${author:20} ${journal:10} ${year:4} ${title:*} ${=type=:3}")))
    (setq ivy-bibtex-default-action 'ivy-bibtex-insert-key)
    (ivy-add-actions 'ivy-bibtex '(("SPC" ivy-bibtex-quicklook "Quick look")))
    (setq bibtex-completion-format-citation-functions
          '((org-mode . bibtex-completion-format-citation-pandoc-citeproc)
            (default . bibtex-completion-format-citation-default))
          bibtex-completion-bibliography "~/Dropbox/org/reference/Bibliography.bib"
          bibtex-completion-library-path "~/Dropbox/org/reference/pdf/"
          bibtex-completion-notes-path "~/Dropbox/org/ref.org"
          bibtex-completion-pdf-field "file"
          bibtex-completion-pdf-open-function (lambda (fpath) (start-process "open" "*open*" "open" fpath))))
  (setq bibtex-dialect 'BibTeX
        org-ref-clean-bibtex-entry-hook '(org-ref-bibtex-format-url-if-doi
                                          orcb-key-comma
                                          org-ref-replace-nonascii
                                          orcb-&
                                          orcb-%
                                          org-ref-title-case-article
                                          orcb-clean-year
                                          +reference*org-ref-key
                                          ;; orcb-key
                                          orcb-clean-doi
                                          orcb-clean-pages
                                          orcb-check-journal
                                          org-ref-sort-bibtex-entry)
        org-ref-default-bibliography '("~/Dropbox/org/reference/Bibliography.bib")
        org-ref-bibliography-notes "~/Dropbox/org/ref.org"
        org-ref-pdf-directory "~/Dropbox/org/reference/pdf/"
        org-ref-get-pdf-filename-function (lambda (key) (car (bibtex-completion-find-pdf key)))
        org-ref-notes-function (lambda (thekey)
                                 (let* ((results (org-ref-get-bibtex-key-and-file thekey))
                                        (key (car results))
                                        (bibfile (cdr results)))

                                   (save-excursion
                                     (with-temp-buffer
                                       (insert-file-contents bibfile)
                                       (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
                                       (bibtex-search-entry key)
                                       (org-ref-open-bibtex-notes)))))
        org-ref-create-notes-hook '((lambda ()
                                      (org-narrow-to-subtree)
                                      (insert (format "cite:%s\n" (org-entry-get (point) "CUSTOM_ID")))))
        org-ref-note-title-format "* TODO %t
 :PROPERTIES:
  :CUSTOM_ID: %k
 :END:
")
;; * advice
  (advice-add 'org-ref-ivy-insert-cite-link :override #'+reference*org-ref-ivy-insert-cite-link)
  (ivy-set-actions 'org-ref-ivy-insert-cite-link org-ref-ivy-cite-actions)
  (ivy-set-display-transformer 'org-ref-ivy-insert-cite-link 'ivy-bibtex-display-transformer)
  (advice-add 'org-ref-bib-citation :override #'+reference*org-ref-bib-citation)
  (advice-add 'org-ref-email-bibtex-entry :override #'+reference*org-ref-email-bibtex-entry))




