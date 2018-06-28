;; modules/tools/reference/autoload/org-ref-ivy.el -*- lexical-binding: t; -*-
;; * batch
;;;###autoload
(defun +reference/bibtex-wash--bibtex ()
  (interactive)
  (while (parsebib-find-next-item)
    (if (and (not (string-equal
                   "bioRxiv"
                   (bibtex-completion-get-value
                    "journal"
                    (bibtex-completion-get-entry
                     (bibtex-completion-key-at-point)))))
             (not (string-equal
                   "bioRxiv"
                   (bibtex-completion-get-value
                    "location"
                    (bibtex-completion-get-entry
                     (bibtex-completion-key-at-point)))))
             (not (string-equal
                   "arXiv.org"
                   (bibtex-completion-get-value
                    "journal"
                    (bibtex-completion-get-entry
                     (bibtex-completion-key-at-point)))))
             (not (string-equal
                   ""
                   (bibtex-completion-get-value
                    "journal"
                    (bibtex-completion-get-entry
                     (bibtex-completion-key-at-point)))))
             (not (string-equal
                   ""
                   (bibtex-completion-get-value
                    "doi"
                    (bibtex-completion-get-entry
                     (bibtex-completion-key-at-point))))))
        (progn
          (condition-case nil
              (call-interactively
               'doi-utils-update-bibtex-entry-from-doi)
            (error t))
          (parsebib-find-next-item)))))
;;;###autoload
(defun +reference/bibtex-wash--biorxiv ()
  (interactive)
  (while (parsebib-find-next-item)
    (if (and (or (string-equal
                  "bioRxiv"
                  (bibtex-completion-get-value
                   "journal"
                   (bibtex-completion-get-entry
                    (bibtex-completion-key-at-point))))
                 (string-equal
                  "bioRxiv"
                  (bibtex-completion-get-value
                   "location"
                   (bibtex-completion-get-entry
                    (bibtex-completion-key-at-point)))))
             (not (string-equal
                   ""
                   (bibtex-completion-get-value
                    "url"
                    (bibtex-completion-get-entry
                     (bibtex-completion-key-at-point))))))
        (progn
          (condition-case nil
              (call-interactively
               '+reference/biorxiv-update-bibtex)
            (error t))
          (parsebib-find-next-item))
      (parsebib-find-next-item))))
;;;###autoload
(defun +reference/bibtex-get-all-pdf ()
  (interactive)
  (while (parsebib-find-next-item)
    (if (not (bibtex-completion-find-pdf
              (bibtex-completion-key-at-point)))
        (progn
          (call-interactively
           'doi-utils-get-bibtex-entry-pdf)
          (parsebib-find-next-item))
      (parsebib-find-next-item))))

(when IS-MAC
;;;###autoload
  (defun +reference/skim-get-annotation ()
    (interactive)
    (message
     "Applescript: Getting Skim page link...")
    (org-mac-paste-applescript-links
     (+reference/clean-skim-page-link
      (+reference/get-skim-page-link))))
;;;###autoload
  (defun +reference/org-mac-skim-insert-page ()
    (interactive)
    (insert
     (+reference/skim-get-annotation)))
;;;###autoload
  (defun +reference/org-ref-find-entry-in-notes (key)
    "Find or create bib note for KEY"
    (let* ((entry (bibtex-completion-get-entry
                   key)))
      (widen)
      (goto-char (point-min))
      (unless (derived-mode-p 'org-mode)
        (error
         "Target buffer \"%s\" for jww/find-journal-tree should be in Org mode"
         (current-buffer)))
      (let* ((headlines (org-element-map
                            (org-element-parse-buffer)
                            'headline
                          'identity))
             (keys (mapcar
                    (lambda (hl)
                      (org-element-property
                       :CUSTOM_ID hl))
                    headlines)))
        ;; put new entry in notes if we don't find it.
        (if (-contains? keys key)
            (progn
              (org-open-link-from-string
               (format "[[#%s]]" key))
              (lambda nil
                (cond ((org-at-heading-p)
                       (org-beginning-of-line))
                      (t
                       (org-previous-visible-heading
                        1)))))
          ;; no entry found, so add one
          (goto-char (point-max))
          (insert
           (org-ref-reftex-format-citation
            entry
            (concat
             "\n"
             org-ref-note-title-format)))
          (mapc
           (lambda (x)
             (save-restriction
               (save-excursion (funcall x))))
           org-ref-create-notes-hook)
          (org-open-link-from-string
           (format "[[#%s]]" key))
          (lambda nil
            (cond ((org-at-heading-p)
                   (org-beginning-of-line))
                  (t
                   (org-previous-visible-heading
                    1))))))))
;;;###autoload
  (defun +reference/org-move-point-to-capture-skim-annotation ()
    (let* ((keystring (+reference/skim-get-bibtex-key)))
      (+reference/org-ref-find-entry-in-notes
       keystring)))

;;;###autoload
  (defun bibtex-completion-quicklook (keys)
    "Open the associated URL or DOI in a browser."
    (dolist (key keys)
      (let* ((pdf (car (bibtex-completion-find-pdf
                        key
                        bibtex-completion-find-additional-pdfs))))
        (start-process
         "Live Preview"
         nil
         "/usr/bin/qlmanage"
         "-p"
         pdf)))))
