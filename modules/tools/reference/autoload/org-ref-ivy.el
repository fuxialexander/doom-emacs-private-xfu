;; modules/tools/reference/autoload/org-ref-ivy.el -*- lexical-binding: t; -*-
;;;###autoload
(defun org-ref-update-bibtex-from-biorxiv (url)
  (interactive (list
                (or (bibtex-autokey-get-field "url")
                    (read-string "URL: "))))
  nil
  (bibtex-narrow-to-entry)
  (bibtex-beginning-of-entry)
  (let ((oldkey (bibtex-completion-key-at-point)))
    (call-interactively 'mark-whole-buffer)
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
;;;###autoload
(defun org-ref-add-bibtex-entry-from-biorxiv (url &optional bibfile)
  (interactive (list
                (or (ignore-errors (bibtex-autokey-get-field "url"))
                    (read-string "URL: "))))
  ;; nil
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
  (save-window-excursion
    (with-current-buffer
        (find-file-noselect bibfile)
      ;; Check if the url already exists
      (goto-char (point-min))
      (if (word-search-forward (concat url) nil t)
          (message "%s is already in this file" url)
        (goto-char (point-max))
        ;; make sure we are at the beginning of a line
        (when (not (= (point) (line-beginning-position)))
          (forward-char 1))
        (when (not (looking-back "\n\n" 3))
          (insert "\n\n"))
        (when (not (looking-back "\n\n" (min 3 (point))))
          (insert "\n\n"))
        (org-ref-insert-bibtex-entry-from-biorxiv url)
        (save-buffer))
      )))
;;;###autoload
(defun org-ref-insert-bibtex-entry-from-biorxiv (url)
  "Insert bibtex entry from a DOI.
Also cleans entry using ‘org-ref’, and tries to download the corresponding pdf."
  (org-ref-get-bibtex-string-from-biorxiv url)
  (backward-char)
  ;; set date added for the record
  (when doi-utils-timestamp-format-function
    (org-ref-bibtex-set-field
     doi-utils-timestamp-field
     (funcall doi-utils-timestamp-format-function)))
  ;; Clean the record
  (org-ref-clean-bibtex-entry)
  ;; try to get pdf
  (when doi-utils-download-pdf
    (org-ref-get-pdf-from-biorxiv-eprint))
  ;; Make notes
  (when (and doi-utils-make-notes org-ref-bibliography-notes)
    (save-excursion
      (when (f-file? org-ref-bibliography-notes)
        (find-file-noselect org-ref-bibliography-notes)
        (save-buffer))
      (let ((bibtex-files (list (buffer-file-name))))
        (funcall doi-utils-make-notes-function))))
  )
;;;###autoload
(defun org-ref-get-bibtex-string-from-biorxiv (biorxivurl)
  ;; <a href="/highwire/citation/73994/bibtext"
  (require 'url-handlers)
  (setq *doi-utils-waiting* t)
  (url-retrieve
   biorxivurl
   (lambda (cbargs)
     (goto-char (point-min))
     (re-search-forward "<a href=\"\\(/highwire/citation/.*/bibtext\\)\"" nil t)
     (setq *doi-utils-biorxiv-bibtex-url* (concat "https://www.biorxiv.org" (match-string 1))
           *doi-utils-waiting* nil)))
  (while *doi-utils-waiting* (sleep-for 0.1))
  (goto-char (point-max))
  (url-insert
   (url-retrieve-synchronously *doi-utils-biorxiv-bibtex-url*)))
;;;###autoload
(defun org-ref-wash-bib ()
  (interactive)
  (while (parsebib-find-next-item)
    (if
        (and
         (not (string-equal "bioRxiv"
                            (bibtex-completion-get-value "journal"
                                                         (bibtex-completion-get-entry (bibtex-completion-key-at-point)))))
         (not (string-equal "bioRxiv"
                            (bibtex-completion-get-value "location"
                                                         (bibtex-completion-get-entry (bibtex-completion-key-at-point)))))
         (not (string-equal "arXiv.org"
                            (bibtex-completion-get-value "journal"
                                                         (bibtex-completion-get-entry (bibtex-completion-key-at-point)))))
         (not (string-equal ""
                            (bibtex-completion-get-value "journal"
                                                         (bibtex-completion-get-entry (bibtex-completion-key-at-point)))))
         (not (string-equal ""
                            (bibtex-completion-get-value "doi"
                                                         (bibtex-completion-get-entry (bibtex-completion-key-at-point))))))
        (progn
          (condition-case nil
              (call-interactively 'doi-utils-update-bibtex-entry-from-doi)
            (error t))
          (parsebib-find-next-item)
          ))))
;;;###autoload
(defun org-ref-wash-biorxiv ()
  (interactive)
  (while (parsebib-find-next-item)
    (if
        (and
         (or (string-equal "bioRxiv" (bibtex-completion-get-value "journal" (bibtex-completion-get-entry (bibtex-completion-key-at-point))))
             (string-equal "bioRxiv" (bibtex-completion-get-value "location" (bibtex-completion-get-entry (bibtex-completion-key-at-point)))))
         (not (string-equal "" (bibtex-completion-get-value "url" (bibtex-completion-get-entry (bibtex-completion-key-at-point))))))
        (progn
          (condition-case nil
              (call-interactively 'org-ref-update-bibtex-from-bioRxiv)
            (error t))
          (parsebib-find-next-item)
          )
      (parsebib-find-next-item)
      )))
;;;###autoload
(defun my-org-ref-key ()
  "Replace the key in the entry."
  (let ((oldkey (bibtex-completion-key-at-point))
        (pdf (bibtex-completion-find-pdf (bibtex-completion-key-at-point)))
        (key (funcall org-ref-clean-bibtex-key-function
                      (bibtex-generate-autokey))))
    ;; first we delete the existing key
    (if pdf (mapcar #'(lambda (x) (condition-case nil (rename-file x (downcase (replace-regexp-in-string oldkey key x))) (error t))) pdf))
    (bibtex-beginning-of-entry)
    (re-search-forward bibtex-entry-maybe-empty-head)
    (if (match-beginning bibtex-key-in-head)
        (delete-region (match-beginning bibtex-key-in-head)
                       (match-end bibtex-key-in-head)))
    ;; check if the key is in the buffer
    (when (save-excursion
            (bibtex-search-entry key))
      (save-excursion
        (bibtex-search-entry key)
        (bibtex-copy-entry-as-kill)
        (switch-to-buffer-other-window "*duplicate entry*")
        (bibtex-yank))
      (setq key (bibtex-read-key "Duplicate Key found, edit: " key)))
    (insert key)
    (kill-new key)))
;;;###autoload
(defun org-ref-get-all-pdf ()
  (interactive)
  (while (parsebib-find-next-item)
    (if (not (bibtex-completion-find-pdf (bibtex-completion-key-at-point)))
        (progn
          (call-interactively 'doi-utils-get-bibtex-entry-pdf)
          (parsebib-find-next-item)
          )
      (parsebib-find-next-item))))

;;;###autoload
(defun +reference/skim-get-annotation ()
  (interactive)
  (message "Applescript: Getting Skim page link...")
  (org-mac-paste-applescript-links (+reference/clean-skim-page-link (+reference/get-skim-page-link))))
;;;###autoload
(defun +reference/org-mac-skim-insert-page ()
  (interactive)
  (insert (+reference/skim-get-annotation)))

;;;###autoload
(defun +reference/org-ref-find-entry-in-notes (key)
  "Find or create bib note for KEY"
  (let* ((entry (bibtex-completion-get-entry key)))
    (widen)
    (goto-char (point-min))
    (unless (derived-mode-p 'org-mode)
      (error
       "Target buffer \"%s\" for jww/find-journal-tree should be in Org mode"
       (current-buffer)))
    (let* ((headlines (org-element-map
                          (org-element-parse-buffer)
                          'headline 'identity))
           (keys (mapcar
                  (lambda (hl) (org-element-property :CUSTOM_ID hl))
                  headlines)))
      ;; put new entry in notes if we don't find it.
      (if (-contains? keys key)
          (progn
            (org-open-link-from-string (format "[[#%s]]" key))
            (lambda nil
              (cond ((org-at-heading-p)
                     (org-beginning-of-line))
                    (t (org-previous-visible-heading 1))))
            )
        ;; no entry found, so add one
        (goto-char (point-max))
        (insert (org-ref-reftex-format-citation
                 entry (concat "\n" org-ref-note-title-format)))
        (mapc (lambda (x)
                (save-restriction
                  (save-excursion
                    (funcall x))))
              org-ref-create-notes-hook)
        (org-open-link-from-string (format "[[#%s]]" key))
        (lambda nil
          (cond ((org-at-heading-p)
                 (org-beginning-of-line))
                (t (org-previous-visible-heading 1))))
        ))))

;;;###autoload
(defun +reference/org-move-point-to-capture-skim-annotation ()
  (let* ((keystring (+reference/skim-get-bibtex-key)))
    (+reference/org-ref-find-entry-in-notes keystring)))
;;;###autoload
(defun org-ref-add-bibtex-entry-from-elfeed-entry ()
  "Add elfeed entry to bibtex."
  (interactive)
  (let ((url (elfeed-entry-link elfeed-show-entry)))
    (if (string-match ".*biorxiv" url)
        (org-ref-add-bibtex-entry-from-biorxiv url)
      (doi-utils-add-entry-from-elfeed-entry))))
;;;###autoload
(defun doi-utils-add-entry-from-elfeed-entry ()
  "Add elfeed entry to bibtex."
  (interactive)
  (let* (;;(title (elfeed-entry-title elfeed-show-entry))
         (url (elfeed-entry-link elfeed-show-entry))
         (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
         (entry-id (elfeed-entry-id elfeed-show-entry)))
    (if (string-match "DOI: \\(.*\\)$" content)
        (doi-utils-add-bibtex-entry-from-doi (match-string 1 content))
      (let ((dois (org-ref-url-scrape-dois url)))
        (cond
         ;; One doi found. Assume it is what we want.
         ((= 1 (length dois))
          (doi-utils-add-bibtex-entry-from-doi (car dois))
          )
         ;; Multiple DOIs found
         ((> (length dois) 1)
          (ivy-read
           "Select a DOI"
           `(candidates . ,(let ((dois '()))
                             (with-current-buffer (url-retrieve-synchronously url)
                               (cl-loop for doi-pattern in org-ref-doi-regexps
                                        do
                                        (goto-char (point-min))
                                        (while (re-search-forward doi-pattern nil t)
                                          (cl-pushnew
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
           :action 'doi-utils-add-bibtex-entry-from-doi)
          (bibtex-beginning-of-entry)
          (delete-char -2)))))))


;;;###autoload
(defun org-ref-email-bibtex-entry ()
  "Email current bibtex entry at point and pdf if it exists."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* (
           (key (reftex-get-bib-field "=key=" (bibtex-parse-entry t)))
           (entry (bibtex-completion-get-entry key))
           (title (bibtex-completion-get-value "title" entry))
           (citation (bibtex-completion-apa-format-reference key))
           pdf)
      ;; when we have org-ref defined we may have pdf to find.
      (when (boundp 'org-ref-pdf-directory)
        (setq pdf (expand-file-name
                   (concat key ".pdf")
                   org-ref-pdf-directory)))
      (compose-mail)
      (message-goto-body)
      (insert citation "\n")
      (message-goto-subject)
      (insert "Paper Sharing: " title)
      (message "%s exists %s" pdf (file-exists-p pdf))
      (when (file-exists-p pdf)
        (mml-attach-file pdf))
      (message-goto-to))))

;;;###autoload
(defun org-ref-bib-citation ()
  "From a bibtex entry, create and return a citation string."
  (bibtex-completion-apa-format-reference (org-ref-get-bibtex-key-under-cursor)))

(defun nature-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "^http://www.nature.com" *doi-utils-redirect*)
    (let ((result *doi-utils-redirect*))
      ;; (setq result (replace-regexp-in-string "/full/" "/pdf/" result))
      (concat result "\.pdf"))))

(defun biorxiv-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "www.biorxiv.org"
                      *doi-utils-redirect*)
    (replace-regexp-in-string "early" "biorxiv/early" (concat *doi-utils-redirect* ".full.pdf"))))

(defun bmc-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match "biomedcentral.com" *doi-utils-redirect*)
    (let ((url (downcase *doi-utils-redirect*)))
      (setq url (replace-regexp-in-string "articles" "track/pdf" url))
      url)))

(defun oup-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (while *doi-utils-waiting* (sleep-for 0.1))
  (when (string-match "academic.oup.com" *doi-utils-redirect*)
    (doi-utils-get-oup-pdf-url *doi-utils-redirect*)
    *doi-utils-pdf-url*))

(defun doi-utils-get-oup-pdf-url (redirect-url)
  "Science direct hides the pdf url in html.  We get it out here.
REDIRECT-URL is where the pdf url will be in."
  (setq *doi-utils-waiting* t)
  (url-retrieve
   redirect-url
   (lambda (status)
     (goto-char (point-min))
     (re-search-forward "citation_pdf_url\" content=\"\\([^\"]*\\)\"" nil t)
     (setq *doi-utils-pdf-url* (match-string 1)
           *doi-utils-waiting* nil)
     ))
  (while *doi-utils-waiting* (sleep-for 0.1))
  *doi-utils-pdf-url*)


;; (defun generic-full-pdf-url (*doi-utils-redirect*)
;;   "Get url to the pdf from *DOI-UTILS-REDIRECT*."
;;   (when (or
;;          (string-match "^http://www.jneurosci.org" *doi-utils-redirect*)
;;          (string-match "cshlp.org" *doi-utils-redirect*))
;;     (concat *doi-utils-redirect* ".full.pdf")))

;;;###autoload
(defun generic-as-get-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (do-applescript (concat "
tell application \"Google Chrome\"
activate
set myTab to make new tab at end of tabs of window 1
set URL of myTab to \""
                          *doi-utils-redirect*
                          "\"
end tell
"))
  (do-applescript "
set question to display dialog \"Locate PDF URL\" buttons {\"OK\"} default button 1
tell application \"Google Chrome\"
    if button returned of question is \"OK\" then
        return URL of active tab of front window
    end if
end tell
"))


;;;###autoload
(defun org-ref-get-pdf-from-biorxiv-eprint ()
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((pdf (bibtex-autokey-get-field "eprint"))
         (key (cdr (assoc "=key=" (bibtex-parse-entry))))
         (pdf-file (concat (if org-ref-pdf-directory
                               (file-name-as-directory org-ref-pdf-directory)
                             (read-directory-name "PDF directory: " "."))
                           key ".pdf")))
    (unless (file-exists-p pdf-file)
      (url-copy-file pdf pdf-file))))


;;;###autoload
(defun org-ref-ivy-bibtex-get-pdf-for-entry (entry)
  "Copy selected bibtex ENTRY to the clipboard."
  (with-temp-buffer
    (save-window-excursion
      (ivy-bibtex-show-entry entry)
      (doi-utils-get-bibtex-entry-pdf))))

;;;###autoload
(defun org-ref-ivy-bibtex-get-update-for-entry (entry)
  "Copy selected bibtex ENTRY to the clipboard."
  (with-temp-buffer
    (save-window-excursion
      (ivy-bibtex-show-entry entry)
      (call-interactively #'doi-utils-update-bibtex-entry-from-doi))))

;;;###autoload
(defun org-ref-ivy-bibtex-email-entry (entry)
  "Insert selected ENTRY and attach pdf file to an email.
Create email unless called from an email."
  ;; (with-ivy-window
  (let* (
         (key (cdr (assoc "=key=" entry)))
         (entry (bibtex-completion-get-entry key))
         (title (cdr (assoc "title" entry)))
         (citation (bibtex-completion-apa-format-reference key))
         pdf)
    ;; when we have org-ref defined we may have pdf to find.
    (when (boundp 'org-ref-pdf-directory)
      (setq pdf (expand-file-name
                 (concat key ".pdf")
                 org-ref-pdf-directory)))
    (compose-mail)
    (message-goto-body)
    (insert citation "\n")
    (message-goto-subject)
    (insert "Paper Sharing: " title)
    (message "%s exists %s" pdf (file-exists-p pdf))
    (when (file-exists-p pdf)
      (mml-attach-file pdf))
    (message-goto-to))
  ;; )
  )

;;;###autoload
(defun org-ref-ivy-bibtex-insert-formatted-citation (entry)
  "Insert formatted citations at point for selected ENTRY."
  (with-ivy-window
    (insert (bibtex-completion-apa-format-reference (bibtex-completion-get-value "=key=" entry)))))

;;;###autoload
(defun org-ref-ivy-bibtex-copy-formatted-citation (entry)
  "Copy formatted citation to clipboard for ENTRY."
  (kill-new (bibtex-completion-apa-format-reference (bibtex-completion-get-value "=key=" entry))))


;;;###autoload
(defun bibtex-completion-quicklook (keys)
  "Open the associated URL or DOI in a browser."
  (dolist (key keys)
    (let* ((pdf (car (bibtex-completion-find-pdf key bibtex-completion-find-additional-pdfs))))
      (start-process "Live Preview" nil "/usr/bin/qlmanage" "-p" pdf))))

;; (defun bibtex-completion-open-uri (keys)
;;   "Open the associated URL or DOI in a browser."
;;   (dolist (key keys)
;;     (let* ((entry (bibtex-completion-get-entry key))
;;            (uri (bibtex-completion-get-value "uri" entry))
;;            (uri (s-replace "\\url{" "" uri))
;;            (uri (s-replace "}" "" uri))
;;            )
;;       (start-process "Open URI" nil "/usr/bin/open" uri)
;;       )))
