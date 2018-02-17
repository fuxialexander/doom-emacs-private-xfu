;;; tools/reference/local/org-ref-ivy/org-ref-elfeed.el -*- lexical-binding: t; -*-

(defun doi-utils-add-entry-from-elfeed-entry ()
  "Add elfeed entry to bibtex."
  (interactive)
  (require 'org-ref)
  (let* ((title (elfeed-entry-title elfeed-show-entry))
         (url (elfeed-entry-link elfeed-show-entry))
         (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
         (entry-id (elfeed-entry-id elfeed-show-entry))
         (entry-link (elfeed-entry-link elfeed-show-entry))
         (entry-id-str (concat (car entry-id)
                               "|"
                               (cdr entry-id)
                               "|"
                               url)))
    (if (string-match "DOI: \\(.*\\)$" content)
        (doi-add-bibtex-entry (match-string 1 content)
                              (ido-completing-read
                               "Bibfile: "
                               (append (f-entries "." (lambda (f)
                                                        (and (not (string-match "#" f))
                                                             (f-ext? f "bib"))))
                                       org-ref-default-bibliography)))
      (let ((dois (org-ref-url-scrape-dois url)))
        (cond
         ;; One doi found. Assume it is what we want.
         ((= 1 (length dois))
          (doi-utils-add-bibtex-entry-from-doi
           (car dois)
           (ido-completing-read
            "Bibfile: "
            (append (f-entries "." (lambda (f)
                                     (and (not (string-match "#" f))
                                          (f-ext? f "bib"))))
                    org-ref-default-bibliography)))
          action)
         ;; Multiple DOIs found
         ((> (length dois) 1)
          (helm :sources
                `((name . "Select a DOI")
                  (candidates . ,(let ((dois '()))
                                   (with-current-buffer (url-retrieve-synchronously url)
                                     (loop for doi-pattern in org-ref-doi-regexps
                                           do
                                           (goto-char (point-min))
                                           (while (re-search-forward doi-pattern nil t)
                                             (pushnew
                                              ;; Cut off the doi, sometimes
                                              ;; false matches are long.
                                              (cons (format "%40s | %s"
                                                            (substring
                                                             (match-string 1)
                                                             0 (min
                                                                (length (match-string 1))
                                                                40))
                                                            doi-pattern)
                                                    (match-string 1))
					                          dois
					                          :test #'equal)))
				                     (reverse dois))))
		          (action . (lambda (candidates)
			                  (let ((bibfile (ido-completing-read
					                          "Bibfile: "
					                          (append (f-entries "." (lambda (f)
								                                       (and (not (string-match "#" f))
									                                        (f-ext? f "bib"))))
						                              org-ref-default-bibliography))))
				                (loop for doi in (helm-marked-candidates)
				                      do
				                      (doi-utils-add-bibtex-entry-from-doi
				                       doi
				                       bibfile)
				                      ;; this removes two blank lines before each entry.
				                      (bibtex-beginning-of-entry)
				                      (delete-char -2)))))))))))))
