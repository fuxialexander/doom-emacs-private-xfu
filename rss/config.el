;;; app/rss/config.el -*- lexical-binding: t; -*-

;; This is an opinionated workflow that turns Emacs into an RSS reader, inspired
;; by apps Reeder and Readkit. It can be invoked via `=rss'. Otherwise, if you
;; don't care for the UI you can invoke elfeed directly with `elfeed'.

(defvar +rss-split-direction 'below
  "What direction to pop up the entry buffer in elfeed.")


;;
;; Packages
;;

(def-package! elfeed
  :commands elfeed
  :config
  (setq elfeed-search-filter "@1-week-ago +unread"
        elfeed-db-directory (concat doom-local-dir "elfeed/db/")
        elfeed-enclosure-default-dir (concat doom-local-dir "elfeed/enclosures/")
        elfeed-search-title-min-width 80
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'+rss/delete-pane
        shr-max-image-proportion 0.6)
  (make-directory elfeed-db-directory t)

  ;; Ensure elfeed buffers are treated as real
  (push (lambda (buf) (string-match-p "^\\*elfeed" (buffer-name buf)))
        doom-real-buffer-functions)

  ;; Enhance readability of a post
  (add-hook 'elfeed-show-mode-hook #'+rss|elfeed-wrap)

  (after! elfeed-search
    (push 'elfeed-search-mode evil-snipe-disabled-modes)
    (set! :evil-state 'elfeed-search-mode 'normal))
  (after! elfeed-show
    (push 'elfeed-show-mode evil-snipe-disabled-modes)
    (set! :evil-state 'elfeed-show-mode 'normal))

;; ;;;; function overreide
  (defun elfeed-search-print-entry--default (entry)
    "Print ENTRY to the buffer."
    (let* ((elfeed-goodies/wide-threshold 0.5)
           (elfeed-goodies/tag-column-width 40)
           (elfeed-goodies/feed-source-column-width 30)
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (concat (mapconcat 'identity tags ",")))
           (title-width (- (window-width) elfeed-goodies/feed-source-column-width
                           elfeed-goodies/tag-column-width 4))
           (title-column (elfeed-format-column
                          title (elfeed-clamp
                                 elfeed-search-title-min-width
                                 title-width
                               title-width)
                        :left))
         (tag-column (elfeed-format-column
                      tags-str (elfeed-clamp (length tags-str)
                                             elfeed-goodies/tag-column-width
                                             elfeed-goodies/tag-column-width)
                      :left))
         (feed-column (elfeed-format-column
                       feed-title (elfeed-clamp elfeed-goodies/feed-source-column-width
                                                elfeed-goodies/feed-source-column-width
                                                elfeed-goodies/feed-source-column-width)
                       :left)))

    (if (>= (window-width) (* (frame-width) elfeed-goodies/wide-threshold))
        (progn
          (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
          (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
          (insert (propertize title 'face title-faces 'kbd-help title)))
      (insert (propertize title 'face title-faces 'kbd-help title))))
    )
  (defun elfeed-show-entry (entry)
    "Display ENTRY in the current buffer."
    (let ((buff (get-buffer-create "*elfeed-entry*")))
      (+rss-popup-pane buff)
      (with-current-buffer buff
        (elfeed-show-mode)
        (setq elfeed-show-entry entry)
        (elfeed-show-refresh))))
  (defun elfeed-adjust-show-entry (entry)
    "Display ENTRY in the current buffer."
    (let ((buff (get-buffer-create "*elfeed-entry*")))
      (+rss-popup-pane buff)
      (with-current-buffer buff
        (elfeed-show-mode)
        (setq elfeed-show-entry entry)
        (elfeed-show-refresh))))
  (defun elfeed-search-adjust-show-entry (entry)
    "Display the currently selected item in a buffer."
    (interactive (list (elfeed-search-selected :ignore-region)))
    (require 'elfeed-show)
    (when (elfeed-entry-p entry)
      (elfeed-untag entry 'unread)
      (elfeed-search-update-entry entry)
      (elfeed-adjust-show-entry entry)))
  (defun elfeed-show-next ()
    "Show the next item in the elfeed-search buffer."
    (interactive)
    (quit-window)
    (with-current-buffer (elfeed-search-buffer)
      (forward-line)
      (call-interactively #'elfeed-search-adjust-show-entry)))
  (defun elfeed-show-prev ()
    "Show the previous item in the elfeed-search buffer."
    (interactive)
    (quit-window)
    (with-current-buffer (elfeed-search-buffer)
      (forward-line -1)
      (call-interactively #'elfeed-search-adjust-show-entry)))
;;;; ace-elfeed
  (require 'avy)
  (defun ace-link--elfeed-collect ()
    "Collect the positions of visible links in `elfeed' buffer."
    (let (candidates pt)
      (save-excursion
        (save-restriction
          (narrow-to-region
           (window-start)
           (window-end))
          (goto-char (point-min))
          (setq pt (point))
          (while (progn (shr-next-link)
                        (> (point) pt))
            (setq pt (point))
            (when (plist-get (text-properties-at (point)) 'shr-url)
              (push (point) candidates)))
          (nreverse candidates)))))
  (defun ace-link--elfeed-action  (pt)
    (goto-char pt)
    (shr-browse-url))
  (defun ace-link-elfeed ()
    "Open a visible link in `elfeed' buffer."
    (interactive)
    (let ((pt (avy-with ace-link-elfeed
                        (avy--process
                         (ace-link--elfeed-collect)
                         #'avy--overlay-pre))))
      (ace-link--elfeed-action pt)))

  (after! org-ref-ivy
    (defun org-ref-doi-utils-add-entry-from-elfeed-entry ()
      "Add elfeed entry to bibtex."
      (interactive)
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
    (defun org-ref-add-bibtex-entry-from-elfeed-entry ()
      "Add elfeed entry to bibtex."
      (interactive)
      (let ((url (elfeed-entry-link elfeed-show-entry)))
        (if (string-match ".*biorxiv" url)
            (org-ref-add-bibtex-entry-from-biorxiv url)
          (org-ref-doi-utils-add-entry-from-elfeed-entry)))))

;;;; Key-binding
  (map!
   (:mode elfeed-search-mode
     (:map elfeed-search-mode-map
       [remap doom/kill-this-buffer] "q"
       [remap kill-this-buffer]      "q"
       [remap kill-buffer]           "q"

       :n "q"   #'+rss/quit
       :n "e"   #'elfeed-update
       :n "r"   #'elfeed-search-untag-all-unread
       :n "u"   #'elfeed-search-tag-all-unread
       :n "s"   #'elfeed-search-live-filter
       :n "RET" #'elfeed-search-show-entry
       :n "+"   #'elfeed-search-tag-all
       :n "-"   #'elfeed-search-untag-all
       :n "S"   #'elfeed-search-set-filter
       :n "o"   #'elfeed-search-browse-url
       :n "y"   #'elfeed-search-yank))
   (:mode elfeed-show-mode
     (:map elfeed-show-mode-map
       [remap kill-this-buffer]      "q"
       [remap kill-buffer]           "q"
       :nm "q" #'+rss/delete-pane
       :nm "o" #'ace-link-elfeed
       ;; :nm "b" #'elfeed-show-send-to-bookend
       :nm "b" #'org-ref-add-bibtex-entry-from-elfeed-entry
       :nm "n" #'elfeed-show-next
       :nm "p" #'elfeed-show-prev
       :nm "+" #'elfeed-show-tag
       :nm "-" #'elfeed-show-untag
       :nm "s" #'elfeed-show-new-live-search
       :nm "y" #'elfeed-show-yank))))

;;;; Elfeed-org
(def-package! elfeed-org
  :after (:all org elfeed)
  :config
  (setq rmh-elfeed-org-files '("~/.emacs.d/modules/private/rss/elfeed.org"))
  (elfeed-org))
