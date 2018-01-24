;;; org-ref-ivy-cite.el --- Use ivy for completion in org-ref

;; Copyright (C) 2016  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'org-ref-bibtex)

(defvar org-ref-ivy-cite-marked-candidates '()
  "Holds entries marked in `org-ref-ivy-insert-cite-link'.")


(defun org-ref-looking-forward-cite ()
  "Return if point is in the position before a citation."
  (save-excursion
    (forward-char)
    (-contains? org-ref-cite-types
                (org-element-property :type (org-element-context)))))


(defun org-ref-looking-back-cite ()
  "Return if point is in the position after a citation."
  (save-excursion
    (forward-char -1)
    (-contains? org-ref-cite-types
                (org-element-property :type (org-element-context)))))


(defun org-ref-ivy-bibtex-insert-cite (entry)
  "Insert a citation for ENTRY.
If `org-ref-ivy-cite-marked-candidates' is non-nil then they are added instead
of ENTRY.  ENTRY is selected from `org-ref-bibtex-candidates'."
  (with-ivy-window
    (if org-ref-ivy-cite-marked-candidates
        (cl-loop for entry in org-ref-ivy-cite-marked-candidates
                 do
                 (if ivy-current-prefix-arg
                     (let ((org-ref-default-citation-link
                            (ivy-read "Type: " org-ref-cite-types)))
                       (org-ref-insert-key-at-point
                        (list (cdr (assoc "=key=" entry)))))
                   (org-ref-insert-key-at-point
                    (list (cdr (assoc "=key=" entry))))))
      (if ivy-current-prefix-arg
          (let ((org-ref-default-citation-link
                 (ivy-read "Type: " org-ref-cite-types)))
            (org-ref-insert-key-at-point (list (cdr (assoc "=key=" entry)))))
        (org-ref-insert-key-at-point (list (cdr (assoc "=key=" entry))))))))


(defun org-ref-ivy-bibtex-open-pdf (entry)
  "Open the pdf associated with ENTRY.
ENTRY is selected from `org-ref-bibtex-candidates'."
  (with-ivy-window
    (let* (
          (key (cdr (assoc "=key=" entry)))
          (pdf-file (car (bibtex-completion-find-pdf key)))
          )
      (if (file-exists-p pdf-file)
          (org-open-file pdf-file)
        (message "No pdf found for %s" key)
        ))))


(defun org-ref-ivy-bibtex-open-notes (entry)
  "Open the notes associated with ENTRY.
ENTRY is selected from `org-ref-bibtex-candidates'."
  (with-ivy-window
    (org-ref-open-notes-at-point (cdr (assoc "=key=" entry)))
    ))


(defun org-ref-ivy-bibtex-open-entry (entry)
  "Open the bibtex file at ENTRY.
ENTRY is selected from `org-ref-bibtex-candidates'."
  (ivy-bibtex-show-entry entry)
  )


(defun org-ref-ivy-bibtex-copy-entry (entry)
  "Copy selected bibtex ENTRY to the clipboard."
  (with-temp-buffer
    (save-window-excursion
      (org-ref-ivy-bibtex-open-entry entry)
      (bibtex-copy-entry-as-kill))
    (bibtex-yank)
    (kill-region (point-min) (point-max))))


(defun org-ref-ivy-bibtex-open-url (entry)
  "Open the URL associated with ENTRY.
ENTRY is selected from `org-ref-bibtex-candidates'."
  (with-ivy-window
  (let* ((url (cdr (assoc "=url=" entry))))
    (if url
        (browse-url url)
      (message "No url found for %s" (cdr (assoc "=key=" entry)))))))


(defun org-ref-ivy-bibtex-open-doi (entry)
  "Open the DOI associated with ENTRY.
ENTRY is selected from `org-ref-bibtex-candidates'."
  (with-ivy-window
  (let* ((doi (cdr (assoc "doi" entry))))
    (if doi
        (browse-url (format "http://dx.doi.org/%s" doi))
      (message "No doi found for %s" (cdr (assoc "=key=" entry)))))))


(defun org-ref-ivy-bibtex-set-keywords (entry)
  "Prompt for keywords, and put them on the selected ENTRY."
  (let ((keywords (read-string "Keyword(s) comma-separated: " ))
        entry-keywords)
    (save-window-excursion
      (org-ref-ivy-bibtex-open-entry entry)
      (setq entry-keywords (bibtex-autokey-get-field "keywords"))
      (org-ref-bibtex-set-field
       "keywords"
       (if (> (length entry-keywords) 0)
           (concat entry-keywords ", " keywords)
         keywords)))))

(defun org-ref-ivy-bibtex-get-pdf-for-entry (entry)
  "Copy selected bibtex ENTRY to the clipboard."
  (with-temp-buffer
    (save-window-excursion
      (org-ref-ivy-bibtex-open-entry entry)
      (org-ref-doi-utils-get-bibtex-entry-pdf))))

(defun org-ref-ivy-bibtex-get-update-for-entry (entry)
  "Copy selected bibtex ENTRY to the clipboard."
  (with-temp-buffer
    (save-window-excursion
      (org-ref-ivy-bibtex-open-entry entry)
      (call-interactively #'org-ref-doi-utils-update-bibtex-entry-from-doi))))


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


(defun org-ref-ivy-bibtex-formatted-citation (entry)
  "Return string containing formatted citations for ENTRY.
This uses a citeproc library."
  (let ((enable-recursive-minibuffers t))
    (ivy-read "Style: " '("unsrt" "author-year")
              :action 'load-library
              :require-match t
              :preselect "unsrt"
              :caller 'org-ref-ivy-formatted-citation)
    (format "%s\n\n" (org-ref-formatted-citation entry))))


(defun org-ref-ivy-bibtex-insert-formatted-citation (entry)
  "Insert formatted citations at point for selected ENTRY."
  (with-ivy-window
    (insert (bibtex-completion-apa-format-reference (bibtex-completion-get-value "=key=" entry)))))


(defun org-ref-ivy-bibtex-copy-formatted-citation (entry)
  "Copy formatted citation to clipboard for ENTRY."
  (kill-new (bibtex-completion-apa-format-reference (bibtex-completion-get-value "=key=" entry))))


(defun org-ref-ivy-bibtex-add-entry (_)
  "Open a bibliography file and move point to the end.
In order to add a new bibtex entry. The arg is selected from
`org-ref-bibtex-candidates' but ignored."
  (ivy-read "bibtex file: " org-ref-bibtex-files
            :require-match t
            :action 'find-file
            :caller 'org-ref-ivy-bibtex-add-entry)
  (widen)
  (goto-char (point-max))
  (unless (bolp)
    (insert "\n")))


(defvar org-ref-ivy-cite-actions
  '(
    ("b"    org-ref-ivy-bibtex-open-entry                 "Open entry")
    ("p"    org-ref-ivy-bibtex-open-pdf                   "Open pdf")
    ("n"    org-ref-ivy-bibtex-open-notes                 "Open notes")
    ("u"    ivy-bibtex-open-url-or-doi                    "Open url or doi")
    ("U"    org-ref-ivy-bibtex-get-update-for-entry       "Update entry from doi")
    ("P"    org-ref-ivy-bibtex-get-pdf-for-entry          "Update PDF for entry")
    ("SPC"  ivy-bibtex-quicklook                          "Quick look")
    ("k"    org-ref-ivy-bibtex-set-keywords               "Add keywords")
    ("e"    org-ref-ivy-bibtex-email-entry                "Email entry")
    ("f"    org-ref-ivy-bibtex-insert-formatted-citation  "Insert formatted citation")
    ("F"    org-ref-ivy-bibtex-copy-formatted-citation    "Copy formatted citation")
    )
  "List of additional actions for `org-ref-ivy-insert-cite-link'.
The default action being to insert a citation.")

(defvar org-ref-ivy-cite-re-builder 'ivy--regex-ignore-order
  "Regex builder to use in `org-ref-ivy-insert-cite-link'.
Can be set to nil to use Ivy's default).")

(defun org-ref-swap (i j lst)
  "Swap index I and J in the list LST."
  (let ((tempi (nth i lst)))
    (setf (nth i lst) (nth j lst))
    (setf (nth j lst) tempi))
  lst)


(defun org-ref-ivy-move-up ()
  "Move ivy candidate up and update candidates."
  (interactive)
  (setf (ivy-state-collection ivy-last)
        (org-ref-swap ivy--index (1- ivy--index)
                      (ivy-state-collection ivy-last)))
  (setf (ivy-state-preselect ivy-last) (ivy-state-current ivy-last))
  (ivy--reset-state ivy-last))


(defun org-ref-ivy-move-down ()
  "Move ivy candidate down."
  (interactive)
  (setf (ivy-state-collection ivy-last)
        (org-ref-swap ivy--index (1+ ivy--index)
                      (ivy-state-collection ivy-last)))
  (setf (ivy-state-preselect ivy-last) (ivy-state-current ivy-last))
  (ivy--reset-state ivy-last))


(defun org-ref-ivy-sort-year-ascending ()
  "Sort entries by year in ascending order."
  (interactive)
  (setf (ivy-state-collection ivy-last)
        (cl-sort (copy-sequence (ivy-state-collection ivy-last))
                 (lambda (a b)
                   (let ((y1 (string-to-number (or (cdr (assoc "year" a)) "0")))
                         (y2 (string-to-number (or (cdr (assoc "year" b)) "0"))))
                     (< y1 y2)))))
  (setf (ivy-state-preselect ivy-last) (ivy-state-current ivy-last))
  (ivy--reset-state ivy-last))


(defun org-ref-ivy-sort-year-descending ()
  "Sort entries by year in descending order."
  (interactive)
  (setf (ivy-state-collection ivy-last)
        (cl-sort (copy-sequence (ivy-state-collection ivy-last))
                 (lambda (a b)
                   (let ((y1 (string-to-number (or (cdr (assoc "year" a)) "0")))
                         (y2 (string-to-number (or (cdr (assoc "year" b)) "0"))))
                     (> y1 y2)))))
  (setf (ivy-state-preselect ivy-last) (ivy-state-current ivy-last))
  (ivy--reset-state ivy-last))


;; * marking candidates
(defun org-ref-ivy-mark-candidate ()
  "Add current candidate to `org-ref-ivy-cite-marked-candidates'.
If candidate is already in, remove it."
  (interactive)
  (let ((cand (or (assoc (ivy-state-current ivy-last)
                         (ivy-state-collection ivy-last))
                  (ivy-state-current ivy-last))))
    (if (-contains? org-ref-ivy-cite-marked-candidates cand)
        ;; remove it from the marked list
        (setq org-ref-ivy-cite-marked-candidates
              (-remove-item cand org-ref-ivy-cite-marked-candidates))
      ;; add to list
      (setq org-ref-ivy-cite-marked-candidates
            (append org-ref-ivy-cite-marked-candidates (list cand)))))
  (ivy-next-line))


(defun org-ref-ivy-show-marked-candidates ()
  "Show marked candidates."
  (interactive)
  (setf (ivy-state-collection ivy-last) org-ref-ivy-cite-marked-candidates)
  (setf (ivy-state-preselect ivy-last) (ivy-state-current ivy-last))
  (ivy--reset-state ivy-last) ivy-last)


(defun org-ref-ivy-show-all ()
  "Show all the candidates."
  (interactive)
  (setf (ivy-state-collection ivy-last)
        (org-ref-bibtex-candidates))
  (ivy--reset-state ivy-last))


;; * org-ref-cite keymap
(defvar org-ref-ivy-cite-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<SPC>") 'org-ref-ivy-mark-candidate)
    (define-key map (kbd "C-,") 'org-ref-ivy-show-marked-candidates)
    (define-key map (kbd "C-.") 'org-ref-ivy-show-all)
    (define-key map (kbd "C-<up>") 'org-ref-ivy-move-up)
    (define-key map (kbd "C-<down>") 'org-ref-ivy-move-down)
    (define-key map (kbd "C-y") 'org-ref-ivy-sort-year-ascending)
    (define-key map (kbd "C-M-y") 'org-ref-ivy-sort-year-descending)
    (define-key map (kbd "C-k") (lambda ()
                                  (interactive)
                                  (beginning-of-line)
                                  (kill-visual-line)
                                  (setf (ivy-state-collection ivy-last)
                                        (org-ref-bibtex-candidates))
                                  (setf (ivy-state-preselect ivy-last)
                                        (ivy-state-current ivy-last))
                                  (ivy--reset-state ivy-last)))
    (define-key map (kbd "C-<return>")
      (lambda ()
        "Apply action and move to next/previous candidate."
        (interactive)
        (ivy-call)
        (ivy-next-line)))
    map)
  "A key map for `org-ref-ivy-insert-cite-link'.")

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
            :action 'org-ref-ivy-bibtex-insert-cite
            :caller 'org-ref-ivy-insert-cite-link))


(ivy-set-display-transformer 'org-ref-ivy-insert-cite-link 'ivy-bibtex-display-transformer)


(defun org-ref-ivy-insert-label-link ()
  "Insert a label with ivy."
  (interactive)
  (insert (concat "label:"
                  (ivy-read "label: " (org-ref-get-labels)))))


(defun org-ref-ivy-insert-ref-link ()
  "Insert a ref link with ivy.
Use a prefix ARG to select the ref type."
  (interactive)
  (let ((label (ivy-read "label: " (org-ref-get-labels) :require-match t)))
    (insert
     (or (when (looking-at "$") " ") "")
     (concat (if ivy-current-prefix-arg
     (ivy-read "type: " org-ref-ref-types)
         org-ref-default-ref-type)
       ":"
       label))))


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


(defun org-ref-ivy-onclick-actions ()
  "An alternate click function using ivy for action selection.
Each action is taken from `org-ref-ivy-cite-actions'. Each action should act on
a bibtex entry that matches the key in `org-ref-bibtex-candidates'. Set
`org-ref-cite-onclick-function' to this function to use it."
  (interactive)
  (ivy-read
   "action: "
   (cl-loop for i from 0
            for (_ func s) in
            org-ref-ivy-cite-actions
            collect (cons (format "%2s. %s" i s) func))
   :action (lambda (f)
             (let* ((key (car (org-ref-get-bibtex-key-and-file)))
                    (entry
                     (cdr (elt (org-ref-bibtex-candidates)
                               (-elem-index
                                key
                                (cl-loop for entry in (org-ref-bibtex-candidates)
                                         collect (cdr
                                                  (assoc "=key=" entry ))))))))
               (funcall f entry)))))

;; * org-ref-ivy-set-keywords
(defvar org-ref-ivy-set-keywords-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<SPC>") 'org-ref-ivy-mark-candidate)
    (define-key map (kbd "C-,") 'org-ref-ivy-show-marked-candidates)
    (define-key map (kbd "C-.") 'org-ref-ivy-show-all)
    (define-key map (kbd "C-<up>") 'org-ref-ivy-move-up)
    (define-key map (kbd "C-<down>") 'org-ref-ivy-move-down)
    map)
  "A key map for `org-ref-ivy-set-keywords'.")


(defun org-ref-ivy-set-keywords ()
  "Add keywords to bibtex entries selected by org-ref-ivy."
  (interactive)
  (setq org-ref-ivy-cite-marked-candidates '())
  (ivy-read "Keywords: " (org-ref-bibtex-keywords)
            :keymap org-ref-ivy-set-keywords-keymap
            :caller 'org-ref-ivy-set-keywords
            :action (lambda (key)
                      (org-ref-set-bibtex-keywords
                       (mapconcat 'identity
                                  (or org-ref-ivy-cite-marked-candidates
                                      (list key))
                                  ", ")))))

(ivy-set-display-transformer
 'org-ref-ivy-set-keywords
 'org-ref-ivy-cite-transformer)

(provide 'org-ref-ivy-cite)

;;; org-ref-ivy-cite.el ends here
