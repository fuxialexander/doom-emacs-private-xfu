;;; app/rss/config.el -*- lexical-binding: t; -*-

;; This is an opinionated workflow that turns Emacs into an RSS reader, inspired
;; by apps Reeder and Readkit. It can be invoked via `=rss'. Otherwise, if you
;; don't care for the UI you can invoke elfeed directly with `elfeed'.

(defvar +rss-elfeed-files (list "rss/elfeed.org")
  "The files that configure `elfeed's rss feeds.")

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
        elfeed-show-entry-switch #'+rss-popup-pane
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
;;   (defun elfeed-search-print-entry--default (entry)
;;     "Print ENTRY to the buffer."
;;     (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
;;            (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
;;            (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
;;            (feed (elfeed-entry-feed entry))
;;            (feed-title
;;             (when feed
;;               (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
;;            (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
;;            (tags-str (mapconcat
;;                       (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
;;                       tags "  "))
;;            (title-width (- (window-width) 10 elfeed-search-trailing-width))
;;            (title-column (elfeed-format-column
;;                           title (elfeed-clamp
;;                                  elfeed-search-title-min-width
;;                                  title-width
;;                                  elfeed-search-title-max-width)
;;                           :left)))
;;       (insert (propertize date 'face 'elfeed-search-date-face) "  ")
;;       (insert (propertize title-column 'face title-faces 'kbd-help title) "  ")
;;       (when feed-title
;;         (insert (propertize feed-title 'face 'elfeed-search-feed-face) "  "))

;;       (when tags
;;         (insert "  " tags-str "  "))))

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
       :n "b"   #'elfeed-search-send-to-bookend
       :n "y"   #'elfeed-search-yank))
   (:mode elfeed-show-mode
     (:map elfeed-show-mode-map
       [remap doom/kill-this-buffer] "q"
       [remap kill-this-buffer]      "q"
       [remap kill-buffer]           "q"
       :nm "q" #'elfeed-kill-buffer
       :nm "o" #'ace-link-elfeed
       :nm "b" #'elfeed-show-send-to-bookend
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
  (setq rmh-elfeed-org-files
        (let ((default-directory +org-dir))
          (mapcar #'expand-file-name +rss-elfeed-files)))
  (elfeed-org))
