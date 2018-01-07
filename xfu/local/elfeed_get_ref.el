(defun org-ref-get-biorxiv-from-elfeed ()
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link
      (message "Sent to bookends: %s" link)
      (org-ref-get-bibtex-string-from-biorxiv link)))
  )

(defun org-ref-update-bibtex-from-bioRxiv (url)
      (interactive (list
                    (or (bibtex-autokey-get-field "url")
                        (read-string "URL: "))))
      nil
      (bibtex-narrow-to-entry)
      (bibtex-beginning-of-entry)
      (let ((oldkey (bibtex-completion-key-at-point)))
        (mark-whole-buffer)
        (call-interactively 'delete-region)
        (org-ref-get-bibtex-string-from-biorxiv url)
        (bibtex-beginning-of-entry)
        (re-search-forward bibtex-entry-maybe-empty-head)
        (if (match-beginning bibtex-key-in-head)
            (delete-region (match-beginning bibtex-key-in-head)
                           (match-end bibtex-key-in-head)))
        (insert oldkey)
        (widen))
      (org-ref-clean-bibtex-entry))



(defun elfeed-show-send-to-bookend ()
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (message "Sent to bookends: %s" link)
      (elfeed-send-to-bookend link))))

(defun org-ref-doi-utils-add-bibtex-entry-from-doi (doi &optional bibfile)
  "Add DOI entry to end of a file in the current directory.
Pick the file ending with .bib or in
`org-ref-default-bibliography'.  If you have an active region that
starts like a DOI, that will be the initial prompt.  If no region
is selected and the first entry of the ‘kill-ring’ starts like a
DOI, then that is the intial prompt.  Otherwise, you have to type
or paste in a DOI.
Argument BIBFILE the bibliography to use."
  (interactive
   (list
    (read-string
          "DOI: "
          ;; now set initial input
          (cond
           ;; If region is active and it starts like a doi we want it.
           ((and  (region-active-p)
            (s-match "^10" (buffer-substring (region-beginning) (region-end))))
       (buffer-substring (region-beginning) (region-end)))
      ((and (region-active-p)
            (s-match "^http://dx\\.doi\\.org/"
                     (buffer-substring (region-beginning) (region-end))))
       (replace-regexp-in-string
        "^http://dx\\.doi\\.org/" ""
        (buffer-substring (region-beginning) (region-end))))
      ((and (region-active-p)
            (s-match "^https://dx\\.doi\\.org/"
                     (buffer-substring (region-beginning) (region-end))))
       (replace-regexp-in-string
        "^https://dx\\.doi\\.org/" ""
        (buffer-substring (region-beginning) (region-end))))
      ((and  (region-active-p)
             (s-match (regexp-quote org-ref-doi-utils-dx-doi-org-url)
                      (buffer-substring (region-beginning) (region-end))))
       (replace-regexp-in-string
        (regexp-quote org-ref-doi-utils-dx-doi-org-url) ""
        (buffer-substring (region-beginning) (region-end)))
            (buffer-substring (region-beginning) (region-end)))
           ;; if the first entry in the kill-ring looks
           ;; like a DOI, let's use it.
           ((and
             ;; make sure the kill-ring has something in it
             (stringp (car kill-ring))
             (s-match "^10" (car kill-ring)))
            (car kill-ring))
      ;; maybe kill-ring matches http://dx.doi or somthing
      ((and
        ;; make sure the kill-ring has something in it
        (stringp (car kill-ring))
        (s-match "^http://dx\\.doi\\.org/" (car kill-ring)))
       (replace-regexp-in-string "^http://dx\\.doi\\.org/" "" (car kill-ring)))
      ((and
        ;; make sure the kill-ring has something in it
        (stringp (car kill-ring))
        (s-match "^https://dx\\.doi\\.org/" (car kill-ring)))
       (replace-regexp-in-string "^https://dx\\.doi\\.org/"
                                 "" (car kill-ring)))
      ((and
        ;; make sure the kill-ring has something in it
        (stringp (car kill-ring))
        (s-match (regexp-quote org-ref-doi-utils-dx-doi-org-url)
                 (car kill-ring)))
       (replace-regexp-in-string
        (regexp-quote org-ref-doi-utils-dx-doi-org-url) "" (car kill-ring)))
           ;; otherwise, we have no initial input. You
           ;; will have to type it in.
           (t
            nil)))))

  (unless bibfile
    (setq bibfile (completing-read
                   "Bibfile: "
                   (-uniq
                    (append
                     ;; see if we should add it to a bib-file defined in the file
                     (org-ref-find-bibliography)
                     ;; or any bib-files that exist in the current directory
                     (f-entries "." (lambda (f)
                                      (and (not (string-match "#" f))
                                           (f-ext? f "bib"))))
                     ;; and last in the default bibliography
                     org-ref-default-bibliography)))))
  ;; Wrap in save-window-excursion to restore your window arrangement after this
  ;; is done.
  (save-window-excursion
    (with-current-buffer
        (find-file-noselect bibfile)
      ;; Check if the doi already exists
      (goto-char (point-min))
      (if (word-search-forward (concat doi) nil t)
          (message "%s is already in this file" doi)
        (goto-char (point-max))
        ;; make sure we are at the beginning of a line
        (when (not (= (point) (line-beginning-position)))
          (forward-char 1))
        (when (not (looking-back "\n\n" 3))
          (insert "\n\n"))
        (when (not (looking-back "\n\n" (min 3 (point))))
          (insert "\n\n"))
        (org-ref-doi-utils-insert-bibtex-entry-from-doi doi)
        (save-buffer)))))
