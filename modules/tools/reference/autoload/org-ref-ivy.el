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
  (when org-ref-doi-utils-timestamp-format-function
    (org-ref-bibtex-set-field
     org-ref-doi-utils-timestamp-field
     (funcall org-ref-doi-utils-timestamp-format-function)))
  ;; Clean the record
  (org-ref-clean-bibtex-entry)
  ;; try to get pdf
  (when org-ref-doi-utils-download-pdf
    (org-ref-get-pdf-from-biorxiv-eprint))
  ;; Make notes
  (when (and org-ref-doi-utils-make-notes org-ref-bibliography-notes)
    (save-excursion
      (when (f-file? org-ref-bibliography-notes)
        (find-file-noselect org-ref-bibliography-notes)
        (save-buffer))
      (let ((bibtex-files (list (buffer-file-name))))
        (funcall org-ref-doi-utils-make-notes-function))))
  )
;;;###autoload
(defun org-ref-get-bibtex-string-from-biorxiv (biorxivurl)
  ;; <a href="/highwire/citation/73994/bibtext"
  (require 'url-handlers)
  (setq *org-ref-doi-utils-waiting* t)
  (url-retrieve
   biorxivurl
   (lambda (cbargs)
     (goto-char (point-min))
     (re-search-forward "<a href=\"\\(/highwire/citation/.*/bibtext\\)\"" nil t)
     (setq *org-ref-doi-utils-biorxiv-bibtex-url* (concat "https://www.biorxiv.org" (match-string 1))
           *org-ref-doi-utils-waiting* nil)))
  (while *org-ref-doi-utils-waiting* (sleep-for 0.1))
  (goto-char (point-max))
  (url-insert
   (url-retrieve-synchronously *org-ref-doi-utils-biorxiv-bibtex-url*)))
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
              (call-interactively 'org-ref-doi-utils-update-bibtex-entry-from-doi)
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
          (call-interactively 'org-ref-doi-utils-get-bibtex-entry-pdf)
          (parsebib-find-next-item)
          )
      (parsebib-find-next-item))))
(after! org-mac-link
  (setq org-mac-Skim-highlight-selection-p t)
  ;;;###autoload
  (defun as-get-skim-page-link ()
    (do-applescript
     (concat
      "tell application \"Skim\"\n"
      "set theDoc to front document\n"
      "set theTitle to (name of theDoc)\n"
      "set thePath to (path of theDoc)\n"
      "set thePage to (get index for current page of theDoc)\n"
      "set theSelection to selection of theDoc\n"
      "set theContent to contents of (get text for theSelection)\n"
      "if theContent is missing value then\n"
      "    set theContent to theTitle & \", p. \" & thePage\n"
      (when org-mac-Skim-highlight-selection-p
        (concat
         "else\n"
         "    tell theDoc\n"
         "        set theNote to make note with data theSelection with properties {type:highlight note}\n"
         "         set text of theNote to (get text for theSelection)\n"
         "    end tell\n"))
      "end if\n"
      "set theLink to \"skim://\" & thePath & \"::\" & thePage & "
      "\"::split::\" & theContent\n"
      "end tell\n"
      "return theLink as string\n"))))
;;;###autoload
(defun +reference/skim-get-annotation ()
  (interactive)
  (message "Applescript: Getting Skim page link...")
  (org-mac-paste-applescript-links (+reference/clean-skim-page-link (+reference/get-skim-page-link))))
;;;###autoload
(defun +reference/org-mac-skim-insert-page ()
  (interactive)
  (insert (+reference/skim-get-annotation)))
(after! org
  (org-link-set-parameters "skim" :follow #'+reference/org-mac-skim-open))
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
(after! org-capture
  (defadvice org-capture-finalize
      (after org-capture-finalize-after activate)
    "Advise capture-finalize to close the frame"
    (if (or (equal "SA" (org-capture-get :key))
            (equal "GSA" (org-capture-get :key)))
        (do-applescript "tell application \"Skim\"\n    activate\nend tell")))
  (add-hook 'org-capture-prepare-finalize-hook
            #'(lambda () (if (or (equal "SA" (org-capture-get :key))
                            (equal "GSA" (org-capture-get :key)))
                        (+reference/append-org-id-to-skim (org-id-get-create)))))
  )
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
      (org-ref-doi-utils-add-entry-from-elfeed-entry))))
;;;###autoload
(defun org-ref-doi-utils-add-entry-from-elfeed-entry ()
  "Add elfeed entry to bibtex."
  (interactive)
  (let* (;;(title (elfeed-entry-title elfeed-show-entry))
         (url (elfeed-entry-link elfeed-show-entry))
         (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
         (entry-id (elfeed-entry-id elfeed-show-entry)))
    (if (string-match "DOI: \\(.*\\)$" content)
        (org-ref-doi-utils-add-bibtex-entry-from-doi (match-string 1 content))
      (let ((dois (org-ref-url-scrape-dois url)))
        (cond
         ;; One doi found. Assume it is what we want.
         ((= 1 (length dois))
          (org-ref-doi-utils-add-bibtex-entry-from-doi (car dois))
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
           :action 'org-ref-doi-utils-add-bibtex-entry-from-doi)
          (bibtex-beginning-of-entry)
          (delete-char -2)))))))
(after! elfeed-show
  (map! (:map elfeed-show-mode-map
          :nm "b" #'org-ref-add-bibtex-entry-from-elfeed-entry)))
