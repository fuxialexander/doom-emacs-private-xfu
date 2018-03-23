;; autoload/xfu.el -*- lexical-binding: t; -*-
;;;###autoload
(defun dwim-jump ()
  (interactive)
  (cond ((eq 'org-mode (buffer-local-value 'major-mode (current-buffer)))
         (counsel-org-goto))
        ((eq 'org-agenda-mode (buffer-local-value 'major-mode (current-buffer)))
         (counsel-org-goto-all))
        ((bound-and-true-p outline-minor-mode)
         (counsel-oi))
        (t (counsel-imenu))))

;;;###autoload
(defun mac-iTerm-shell-command (text)
  "Write TEXT into iTerm like user types it with keyboard."
  (interactive
   (list
    (read-shell-command "Run Shell command in iTerm: "
                        (when (use-region-p)
                          (buffer-substring-no-properties
                           (region-beginning)
                           (region-end))))))
  (do-applescript
   (concat
    "tell application \"iTerm\"\n"
    "    activate\n"
    "    create window with default profile\n"
    "    tell current session of current window\n"
    "        write text \"" text "\"\n"
    "    end tell\n"
    "end tell")))
;;;###autoload
(defun mac-iTerm-shell-command-current (text)
  "Write TEXT into iTerm like user types it with keyboard."
  (interactive
   (list
    (read-shell-command "Run Shell command in iTerm: "
                        (when (use-region-p)
                          (buffer-substring-no-properties
                           (region-beginning)
                           (region-end))))))
  (do-applescript
   (concat
    "tell application \"iTerm\"\n"
    "    activate\n"
    "    tell current session of current window\n"
    "        write text \"" text "\"\n"
    "    end tell\n"
    "end tell")))
;;;###autoload
(defun mac-iTerm-cd (dir)
  "Switch to iTerm and change directory there to DIR."
  (interactive (list
                ;; Because shell doesn't expand 'dir'
                (expand-file-name
                 (if current-prefix-arg
                     (read-directory-name "cd to: ")
                   default-directory))))
  (if (file-remote-p dir)
      (let* (
             (host (tramp-file-name-host (tramp-dissect-file-name dir)))
             (dir (tramp-file-name-localname (tramp-dissect-file-name dir)))
             (sshcmd (format "ssh %s" host))
             (cdcmd (format "cd %s" dir))
             )
        (mac-iTerm-shell-command sshcmd)
        (mac-iTerm-shell-command-current cdcmd)
        )
    (let ((cmd (format "cd %s" dir)))
      (mac-iTerm-shell-command cmd))
    )
  )
;;;###autoload
(defun applescript-quote-string (argument)
  "Quote a string for passing as a string to AppleScript."
  (if (or (not argument) (string-equal argument ""))
      "\"\""
    ;; Quote using double quotes, but escape any existing quotes or
    ;; backslashes in the argument with backslashes.
    (let ((result "")
          (start 0)
          end)
      (save-match-data
        (if (or (null (string-match "[^\"\\]" argument))
                (< (match-end 0) (length argument)))
            (while (string-match "[\"\\]" argument start)
              (setq end (match-beginning 0)
                    result (concat result (substring argument start end)
                                   "\\" (substring argument end (1+ end)))
                    start (1+ end))))
        (concat "\"" result (substring argument start) "\"")))))
;;;; Edit
;;;###autoload
(defun highlight-grammar ()
  (interactive)
  (highlight-regexp "\\\w+s[\\\., ;]" 'hi-yellow))

;;;; Org
;;;###autoload
(defun org-agenda-show-daily (&optional arg)
  (interactive "P")
  (org-agenda arg "a"))

;;;###autoload
(defun cfw:open-org-calendar-withoutkevin ()
  (interactive)
  (let ((org-agenda-files '("~/Dropbox/org/" "~/Dropbox/org/cal/cal.org")))
    (call-interactively '+calendar/open-calendar)))

;;;###autoload
(defun cfw:open-org-calendar-withkevin ()
  (interactive)
  (let ((org-agenda-files '("~/Dropbox/org/" "~/Dropbox/org/cal/")))
    (call-interactively '+calendar/open-calendar)))

;;;###autoload
(defun sort-setq-next-record ()
    (condition-case nil
            (progn
                (forward-sexp 1)
                (backward-sexp))
        ('scan-error (end-of-buffer))))

;;;###autoload
(defun sort-setq ()
  (interactive)
  (save-excursion
    (save-restriction
      (let ((sort-end (progn (end-of-defun)
                             (backward-char)
                             (point-marker)))
            (sort-beg (progn (beginning-of-defun)
                             (re-search-forward "[ \\t]*(" (point-at-eol))
                             (forward-sexp)
                             (re-search-forward "\\<" (point-at-eol))
                             (point-marker))))
        (narrow-to-region (1- sort-beg) (1+ sort-end))
        (sort-subr nil #'sort-setq-next-record #'sort-setq-end-record)))))

;;;###autoload
(defun sort-setq-end-record ()
  (condition-case nil
      (forward-sexp 2)
    ('scan-error (end-of-buffer))))


;;;###autoload
(defun +my-workspace/goto-main-window (pname frame)
    (let ((window (car (+my-doom-visible-windows))))
      (if (window-live-p window)
          (select-window window))))

;;;###autoload
(defun +my-doom-visible-windows (&optional window-list)
  "Return a list of the visible, non-popup windows."
  (cl-loop for window in (or window-list (window-list))
           unless (window-dedicated-p window)
           collect window))

;;;###autoload
(defun +my-workspace/close-window-or-workspace ()
  "Close the selected window. If it's the last window in the workspace, close
the workspace and move to the next."
  (interactive)
  (let ((delete-window-fn (if (featurep 'evil) #'evil-window-delete #'delete-window)))
    (if (window-dedicated-p)
        (funcall delete-window-fn)
      (let ((current-persp-name (+workspace-current-name)))
        (cond ((or (+workspace--protected-p current-persp-name)
                   (cdr (+my-doom-visible-windows)))
               (funcall delete-window-fn))
              ((cdr (+workspace-list-names))
               (+workspace/delete current-persp-name)))))))

;;;###autoload
(defun +xfu/browse-private ()
  (interactive) (doom-project-browse "~/.doom.d"))
;;;###autoload
(defun +xfu/find-in-private ()
  (interactive) (doom-project-find-file "~/.doom.d"))

;;;###autoload
(defun +xfu/browse-playground ()
  (interactive) (doom-project-browse "~/Source/playground"))
;;;###autoload
(defun +xfu/find-in-playground ()
  (interactive) (doom-project-find-file "~/Source/playground"))

;;;###autoload
(defun +xfu/browse-work ()
  (interactive) (doom-project-browse "~/Source/work"))
;;;###autoload
(defun +xfu/find-in-work ()
  (interactive) (doom-project-find-file "~/Source/work"))

;;;###autoload
(defun +xfu/browse-snippets ()
  (interactive) (doom-project-browse "~/.doom.d/snippets"))

;;;###autoload
(defun +xfu/find-in-snippets ()
  (interactive) (doom-project-find-file "~/.doom.d/snippets"))
