;; (add-hook 'org-mode-hook #'org-ref-mode)

(def-package! org-ref
  :commands (org-ref-bibtex-next-entry
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
  :init
  (defvar org-ref-ivy-cite-actions
    '(
      ("b"    ivy-bibtex-show-entry                 "Open entry")
      ("p"    ivy-bibtex-open-pdf                   "Open pdf")
      ("n"    ivy-bibtex-edit-notes                         "Open notes")
      ("u"    ivy-bibtex-open-url-or-doi                    "Open url or doi")
      ("U"    org-ref-ivy-bibtex-get-update-for-entry       "Update entry from doi")
      ("P"    org-ref-ivy-bibtex-get-pdf-for-entry          "Update PDF for entry")
      ("SPC"  ivy-bibtex-quicklook                          "Quick look")
      ("k"    org-ref-ivy-set-keywords               "Add keywords")
      ("e"    org-ref-ivy-bibtex-email-entry                "Email entry")
      ("f"    org-ref-ivy-bibtex-insert-formatted-citation  "Insert formatted citation")
      ("F"    org-ref-ivy-bibtex-copy-formatted-citation    "Copy formatted citation"))
    "List of additional actions for `org-ref-ivy-insert-cite-link'.
The default action being to insert a citation.")

  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-ref-doi-utils-pdf-url-functions '(;; 'aps-pdf-url
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
                                              ;; 'science-direct-pdf-url
                                              ;; 'linkinghub-elsevier-pdf-url
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
                                              'generic-full-pdf-url
                                              'generic-as-get-pdf-url
                                              ))
  :config
  (setq bibtex-dialect 'BibTeX
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

        org-ref-get-pdf-filename-function (lambda (key) (car (bibtex-completion-find-pdf key)))
        org-ref-doi-utils-make-notes-function (lambda ()
                                                (bibtex-beginning-of-entry)
                                                (org-ref-bibtex-edit-notes (cdr (assoc "=key="
                                                                                       (bibtex-parse-entry)))))
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
        org-ref-note-title-format "* %t
 :PROPERTIES:
  :CUSTOM_ID: %k
 :END:
")

  (defhydra org-ref-cite-hydra (:color blue :hint nil)
    "
_p_: Open pdf     _w_: WOS          _g_: Google Scholar _K_: Copy citation to clipboard
_u_: Open url     _r_: WOS related  _P_: Pubmed         _k_: Copy key to clipboard
_n_: Open notes   _c_: WOS citing   _C_: Crossref       _f_: Copy formatted entry
_o_: Open entry   _e_: Email entry  ^ ^                 _q_: quit
"
    ("o" org-ref-open-citation-at-point)
    ("p" org-ref-open-pdf-at-point)
    ("n" org-ref-open-notes-at-point)
    ("u" org-ref-open-url-at-point)
    ("w" org-ref-wos-at-point)
    ("r" org-ref-wos-related-at-point)
    ("c" org-ref-wos-citing-at-point)
    ("g" org-ref-google-scholar-at-point)
    ("P" org-ref-pubmed-at-point)
    ("C" org-ref-crossref-at-point)
    ("K" org-ref-copy-entry-as-summary)
    ("k" (progn
           (kill-new
            (car (org-ref-get-bibtex-key-and-file)))))
    ("f" (kill-new
          (bibtex-completion-apa-format-reference (org-ref-get-bibtex-key-under-cursor))))
    ("e" (kill-new (save-excursion
                     (org-ref-open-citation-at-point)
                     (org-ref-email-bibtex-entry))))
    ("q" nil))
  (def-package! ivy-bibtex
    :config
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
  (ivy-set-actions 'org-ref-ivy-insert-cite-link org-ref-ivy-cite-actions)
  (defun org-ref-ivy-insert-cite-link (&optional arg)
    "Ivy function for interacting with bibtex.
Uses `org-ref-find-bibliography' for bibtex sources, unless a
prefix ARG is used, which uses `org-ref-default-bibliography'."
    (interactive "P")
    ;; (setq org-ref-bibtex-files (if arg org-ref-default-bibliography (org-ref-find-bibliography)))
    (when arg (bibtex-completion-clear-cache))
    (bibtex-completion-init)
    ;; (setq org-ref-ivy-cite-marked-candidates '())
    (ivy-read "Open: " (bibtex-completion-candidates)
              :require-match t
              :keymap org-ref-ivy-cite-keymap
              :re-builder org-ref-ivy-cite-re-builder
              :action 'or-ivy-bibtex-insert-cite
              :caller 'org-ref-ivy-insert-cite-link))

  (ivy-set-display-transformer 'org-ref-ivy-insert-cite-link 'ivy-bibtex-display-transformer))




