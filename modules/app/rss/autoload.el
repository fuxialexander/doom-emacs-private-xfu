;;; modules/app/rss/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun =rss ()
  "Activate (or switch to) `elfeed' in its workspace."
  (interactive)
  (unless (featurep! :feature workspaces)
    (user-error ":feature workspaces is required, but disabled"))
  (+workspace-switch "rss" t)
  (if-let* ((buf (cl-find-if (lambda (it) (string-match-p "^\\*elfeed" (buffer-name (window-buffer it))))
                             (doom-visible-windows))))
      (select-window (get-buffer-window buf)) (call-interactively 'elfeed))
  (+workspace/display))

;;;###autoload
(defun +rss/quit ()
  (interactive)
  ;; (doom-kill-matching-buffers "^\\*elfeed")
  ;; (dolist (file rmh-elfeed-org-files) (when-let* ((buf (get-file-buffer file))) (kill-buffer buf)))
  (+workspace/delete "rss"))


;;;###autoload
(defun +rss|elfeed-wrap ()
  "Enhances an elfeed entry's readability by wrapping it to a width of
`fill-column' and centering it with `visual-fill-column-mode'."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (setq-local truncate-lines nil)
    (setq-local shr-width 85)
    (setq-local line-spacing 0.3)
    (setq-local shr-current-font '(:family "charter" :height 1.2))
    (set-buffer-modified-p nil))
  (visual-fill-column-mode))

;;;###autoload
(defun +rss/delete-pane ()
  "Delete the *elfeed-entry* split pane."
  (interactive)
  (let* ((buff (get-buffer "*elfeed-entry*"))
         (window (get-buffer-window buff)))
    (kill-buffer buff)
    (delete-window window)))

;;;###autoload
(defun +rss-popup-pane (buf)
  "Display BUF in a popup."
  (select-window (display-buffer-at-bottom buf '((window-height . 0.6)))))

;;;###autoload
(defun +rss/open (entry)
  "Display the currently selected item in a buffer."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (when (elfeed-entry-p entry)
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (elfeed-show-entry entry)))

;;;###autoload
(defun +rss/next ()
  "Show the next item in the elfeed-search buffer."
  (interactive)
  (funcall elfeed-show-entry-delete)
  (with-current-buffer (elfeed-search-buffer)
    (forward-line)
    (call-interactively '+rss/open)))

;;;###autoload
(defun +rss/previous ()
  "Show the previous item in the elfeed-search buffer."
  (interactive)
  (funcall elfeed-show-entry-delete)
  (with-current-buffer (elfeed-search-buffer)
    (forward-line -1)
    (call-interactively '+rss/open)))

;;;###autoload
(defun +rss-dead-feeds (&optional years)
  "Return a list of feeds that haven't posted anything in YEARS."
  (let* ((years (or years 1.0))
         (living-feeds (make-hash-table :test 'equal))
         (seconds (* years 365.0 24 60 60))
         (threshold (- (float-time) seconds)))
    (with-elfeed-db-visit (entry feed)
      (let ((date (elfeed-entry-date entry)))
        (when (> date threshold)
          (setf (gethash (elfeed-feed-url feed) living-feeds) t))))
    (cl-loop for url in (elfeed-feed-list)
             unless (gethash url living-feeds)
             collect url)))

;; ;;;###autoload
;; (defun elfeed-send-to-bookend (url)
;;   (let ((uri (concat "bookends:///?&beurl=" (url-hexify-string url))))
;;     (start-process "elfeed-bookends" nil "open" "-g" uri)))

;; ;;;###autoload
;; (defun elfeed-search-send-to-bookend ()
;;   (interactive)
;;   (let ((entries (elfeed-search-selected)))
;;     (cl-loop for entry in entries
;;              do (elfeed-untag entry 'unread)
;;              when (elfeed-entry-link entry)
;;              do (elfeed-send-to-bookend it))
;;     (mapc #'elfeed-search-update-entry entries)
;;     (unless (use-region-p) (forward-line))))

;; ;;;###autoload
;; (defun elfeed-show-send-to-bookend ()
;;   (interactive)
;;   (let ((link (elfeed-entry-link elfeed-show-entry)))
;;     (when link
;;       (message "Sent to bookends: %s" link)
;;       (elfeed-send-to-bookend link))))

;;;###autoload
(defun +rss/elfeed-search-print-entry (entry)
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
      (insert (propertize title 'face title-faces 'kbd-help title)))))

;;;###autoload
(defun +rss/elfeed-search--header-1 ()
  "Computes the string to be used as the Elfeed header."
  (cond
   ((zerop (elfeed-db-last-update))
    (elfeed-search--intro-header))
   ((> (elfeed-queue-count-total) 0)
    (let ((total (elfeed-queue-count-total))
          (in-process (elfeed-queue-count-active)))
      (format "%d feeds pending, %d in process"
              (- total in-process) in-process)))
   ((let* ((db-time (seconds-to-time (elfeed-db-last-update)))
           (update (format-time-string "%Y-%m-%d %H:%M" db-time))
           (unread (elfeed-search--count-unread)))
      (format "Updated %s, %s%s"
              (propertize update 'face 'elfeed-search-last-update-face)
              (propertize unread 'face 'elfeed-search-unread-count-face)
              (cond
               (elfeed-search-filter-active "")
               ((string-match-p "[^ ]" elfeed-search-filter)
                (concat ", " (propertize elfeed-search-filter
                                         'face 'elfeed-search-filter-face)))
               ("")))))))

;;;###autoload
(defun +rss/elfeed-search-adjust-show-entry (entry)
  "Display the currently selected item in a buffer."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (when (elfeed-entry-p entry)
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (+rss/elfeed-adjust-show-entry entry)))

;;;###autoload
(defun +rss/elfeed-show-next ()
  "Show the next item in the elfeed-search buffer."
  (interactive)
  (quit-window)
  (with-current-buffer (elfeed-search-buffer)
    (forward-line)
    (call-interactively #'+rss/elfeed-search-adjust-show-entry)))

;;;###autoload
(defun +rss/elfeed-show-prev ()
  "Show the previous item in the elfeed-search buffer."
  (interactive)
  (quit-window)
  (with-current-buffer (elfeed-search-buffer)
    (forward-line -1)
    (call-interactively #'+rss/elfeed-search-adjust-show-entry)))


;;;###autoload
(defun +rss/elfeed-show-entry (entry)
  "Display ENTRY in the current buffer."
  (let ((buff (get-buffer-create "*elfeed-entry*")))
    (+rss-popup-pane buff)
    (with-current-buffer buff
      (elfeed-show-mode)
      (setq elfeed-show-entry entry)
      (elfeed-show-refresh))))

;;;###autoload
(defun +rss/elfeed-adjust-show-entry (entry)
  "Display ENTRY in the current buffer."
  (let ((buff (get-buffer-create "*elfeed-entry*")))
    (+rss-popup-pane buff)
    (with-current-buffer buff
      (elfeed-show-mode)
      (setq elfeed-show-entry entry)
      (elfeed-show-refresh))))


(require 'avy)

;;;###autoload
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

;;;###autoload
(defun ace-link--elfeed-action  (pt)
  (goto-char pt)
  (shr-browse-url))

;;;###autoload
(defun ace-link-elfeed ()
  "Open a visible link in `elfeed' buffer."
  (interactive)
  (let ((pt (avy-with ace-link-elfeed
              (avy--process
               (ace-link--elfeed-collect)
               #'avy--overlay-pre))))
    (ace-link--elfeed-action pt)))

