;;; app/sx/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun *sx-search (site query &optional tags excluded-tags)
  "Display search on SITE for question titles containing QUERY.
When TAGS is given, it is a lists of tags, one of which must
match.  When EXCLUDED-TAGS is given, it is a list of tags, none
of which is allowed to match.

Interactively, the user is asked for SITE and QUERY.  With a
prefix argument, the user is asked for everything."
  (interactive
   (let ((site (sx--maybe-site-prompt current-prefix-arg))
         (query (read-string
                 (format "Query (%s): "
                   (if current-prefix-arg "optional" "mandatory"))
                 ""
                 'sx-search--query-history))
         tags excluded-tags)
     (when (string= query "")
       (setq query nil))
     (when current-prefix-arg
       (setq tags (sx-tag-multiple-read
                   site (concat "Tags" (when query " (optional)"))))
       (unless (or query tags)
         (sx-user-error "Must supply either QUERY or TAGS"))
       (setq excluded-tags
             (sx-tag-multiple-read site "Excluded tags (optional)")))
     (list site query tags excluded-tags)))

  ;; Here starts the actual function
  (sx-initialize)
  (with-current-buffer (get-buffer-create "*sx-search-result*")
    (sx-question-list-mode)
    (setq sx-question-list--next-page-function
          (lambda (page)
            (sx-search-get-questions
             sx-question-list--site page
             query tags excluded-tags
             (cons 'order (if sx-question-list--descending 'desc 'asc))
             (cons 'sort sx-question-list--order))))
    (setq sx-question-list--site site)
    (setq sx-question-list--order sx-search-default-order)
    (setq sx-question-list--order-methods sx-search--order-methods)
    (sx-question-list-refresh 'redisplay)
    (display-buffer (current-buffer))))


;;;###autoload
(defun *sx-question-list--create-question-window ()
  "Create or find a window where a question can be displayed.

If any current window displays a question, that window is
returned. If none do, a new one is created such that the
question-list window remains `sx-question-list-height' lines
high (if possible)."
  (or (sx-question-mode--get-window)
      ;; Create a proper window.
      ;; (let ((window
      ;;        (condition-case er
      ;;            (split-window (selected-window) sx-question-list-height 'below)
      ;;          (error
      ;;           ;; If the window is too small to split, use any one.
      ;;           (if (string-match
      ;;                "Window #<window .*> too small for splitting"
      ;;                (car (cdr-safe er)))
      ;;               (next-window)
      ;;             (error (cdr er)))))))
      ;;   ;; Configure the window to be closed on `q'.
      ;;   (set-window-prev-buffers window nil)
      ;;   (set-window-parameter
      ;;    window 'quit-restore
      ;;    ;; See (info "(elisp) Window Parameters")
      ;;    `(window window ,(selected-window) ,sx-question-mode--buffer))
      ;;   window)
      nil))

;;;###autoload
(defun *goto-sx-question-list (n)
  "goto sx question list buffer"
  (let ((window (get-buffer-window "*sx-search-result*")))
    (if (and (not (eq (buffer-name (current-buffer)) "*sx-search-result*"))
             window)
        (select-window (get-buffer-window "*sx-search-result*")))))

;;;###autoload (autoload 'sx-hydra/body "~/.doom.d/modules/app/sx/autoload.el" nil t)
(defhydra sx-hydra (:color red :hint nil)
    "
┌^^───────────────┐
│ _a_: questions  │
│ _i_: inbox      │
│ _o_: open-link  │
│ _u_: unanswered │
│ _n_: ask        │
│ _s_: search     │
│ _r_: sort       │
└^^───────────────┘
"
    ("a" sx-tab-newest)
    ("A" sx-tab-all-questions)
    ("i" sx-inbox)
    ("o" sx-open-link)
    ("u" sx-tab-unanswered-my-tags)
    ("n" sx-ask)
    ("s" sx-search)
    ("r" sx-question-list-order-by))
