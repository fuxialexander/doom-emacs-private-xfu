;;; tools/reference/config.el -*- lexical-binding: t; -*-

(def-package! org-ref
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
  (defvar +reference-org-ref-ivy-cite-actions
    '(("b" ivy-bibtex-show-entry "Open entry")
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
    "+reference org ref ivy action")

  (when (featurep! :completion helm)
    (setq org-ref-completion-library 'org-ref-helm-bibtex))
  (when (featurep! :completion ivy)
    (setq org-ref-completion-library 'org-ref-ivy-cite))

  :config
  `(setq
    org-ref-default-bibliography (list bibtex-completion-bibliography)
    org-ref-bibliography-notes ,bibtex-completion-notes-path
    org-ref-pdf-directory ,bibtex-completion-library-path
    orhc-bibtex-cache-file (concat doom-cache-dir "org-ref.cache")
    org-ref-clean-bibtex-entry-hook
    '(org-ref-bibtex-format-url-if-doi
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
    org-ref-get-pdf-filename-function
    (lambda (key) (car (bibtex-completion-find-pdf key)))
    org-ref-notes-function
    (lambda (thekey)
      (let* ((results (org-ref-get-bibtex-key-and-file thekey))
             (key (car results))
             (bibfile (cdr results)))
        (save-excursion
          (with-temp-buffer
            (insert-file-contents bibfile)
            (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
            (bibtex-search-entry key)
            (org-ref-open-bibtex-notes)))))
    org-ref-create-notes-hook
    '((lambda ()
        (org-narrow-to-subtree)
        (insert (format "cite:%s\n" (org-entry-get (point) "CUSTOM_ID")))))
    org-ref-note-title-format "* TODO %t
 :PROPERTIES:
  :CUSTOM_ID: %k
 :END:
")
  (when (eq +reference-field 'bioinfo)
    (add-to-list 'doi-utils-pdf-url-functions 'oup-pdf-url)
    (add-to-list 'doi-utils-pdf-url-functions 'bmc-pdf-url)
    (add-to-list 'doi-utils-pdf-url-functions 'biorxiv-pdf-url))
  (when IS-MAC
    (setq doi-utils-pdf-url-functions
          (delete 'generic-full-pdf-url doi-utils-pdf-url-functions))
    (add-to-list 'doi-utils-pdf-url-functions 'generic-as-get-pdf-url t))
  (when (featurep! :completion ivy)
    (require 'ivy-bibtex)
    (after! ivy-bibtex
      (ivy-set-display-transformer 'org-ref-ivy-insert-cite-link 'ivy-bibtex-display-transformer)
      (advice-add 'org-ref-ivy-insert-cite-link :override #'+reference*org-ref-ivy-insert-cite-link)
      (ivy-set-actions 'org-ref-ivy-insert-cite-link +reference-org-ref-ivy-cite-actions)))
  (when (featurep! :app email)
    (advice-add 'org-ref-email-bibtex-entry :override #'+reference*org-ref-email-bibtex-entry))
  (advice-add 'org-ref-bib-citation :override #'+reference*org-ref-bib-citation))


(def-package! bibtex
  :defer t
  :config
  (setq bibtex-dialect 'biblatex
        bibtex-align-at-equal-sign t
        bibtex-text-indentation 20)
  (map! :map bibtex-mode-map
        [fill-paragraph] #'bibtex-fill-entry))


(def-package! bibtex-completion
  :defer t
  :config
  (setq bibtex-completion-format-citation-functions
        '((org-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (latex-mode . bibtex-completion-format-citation-cite)
          (default . bibtex-completion-format-citation-default))
        bibtex-completion-pdf-field "file"
        bibtex-completion-additional-search-fields '("journaltitle")
        bibtex-completion-pdf-symbol "@"
        bibtex-completion-notes-symbol "#"
        bibtex-completion-display-formats '((t . "${=has-pdf=:1}${=has-note=:1} ${author:20} ${journaltitle:10} ${year:4} ${title:*} ${=type=:3}")))
  (cond
   (IS-MAC
    (setq bibtex-completion-pdf-open-function
          (lambda (fpath)
            (async-start-process "open" "*open*" "open" fpath))))
   (IS-LINUX
    (setq bibtex-completion-pdf-open-function
          (lambda (fpath)
            (async-start-process "open-pdf" "/usr/bin/xdg-open" nil fpath))))))

(def-package! ivy-bibtex
  :when (featurep! :completion ivy)
  :commands (ivy-bibtex)
  :config
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-key)
  (ivy-bibtex-ivify-action bibtex-completion-quicklook ivy-bibtex-quicklook)
  (ivy-add-actions 'ivy-bibtex '(("SPC" ivy-bibtex-quicklook "Quick look"))))


(def-package! helm-bibtex
  :when (featurep! :completion helm)
  :commands helm-bibtex)
