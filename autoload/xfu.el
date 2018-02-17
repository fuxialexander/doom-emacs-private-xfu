;; autoload/xfu.el -*- lexical-binding: t; -*-
;;; Functions
;;;; Jump
;;;###autoload
(defun dwim-jump ()
  (interactive)
  (cond ((eq 'org-mode (buffer-local-value 'major-mode (current-buffer)))
            (counsel-org-goto))
        ((bound-and-true-p outline-minor-mode)
         (counsel-oi))
        (t (counsel-imenu))))

;;;; Mac

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
(defun +xfu/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defmacro +xfu--def-browse-in! (name dir)
  (let ((prefix "xfu"))
    `(defun ,(intern (format "+%s/browse-%s" prefix name)) ()
       (interactive)
       (doom-project-browse ,dir))))

;;;###autoload
(defmacro +xfu--def-find-in! (name dir)
  (let ((prefix "xfu"))
    `(defun ,(intern (format "+%s/find-in-%s" prefix name)) ()
       (interactive)
       (doom-project-find-file ,dir))))


;;;###autoload (autoload '+xfu/browse-project "autoload/xfu" nil t)
(+xfu--def-browse-in! project (doom-project-root))

;;;###autoload (autoload '+xfu/find-in-templates "autoload/xfu" nil t)
(+xfu--def-find-in!   templates +file-templates-dir)
;;;###autoload (autoload '+xfu/browse-templates "autoload/xfu" nil t)
(+xfu--def-browse-in! templates +file-templates-dir)

;;;###autoload (autoload '+xfu/find-in-doom "autoload/xfu" nil t)
(+xfu--def-find-in!   doom doom-emacs-dir)
;;;###autoload (autoload '+xfu/browse-doom "autoload/xfu" nil t)
(+xfu--def-browse-in! doom doom-emacs-dir)

;;;###autoload (autoload '+xfu/find-in-org "autoload/xfu" nil t)
(+xfu--def-find-in!   org +org-dir)
;;;###autoload (autoload '+xfu/browse-org "autoload/xfu" nil t)
(+xfu--def-browse-in! org +org-dir)

;;;###autoload (autoload '+xfu/find-in-private "autoload/xfu" nil t)
(+xfu--def-find-in! private +private-config-path)
;;;###autoload (autoload '+xfu/browse-private "autoload/xfu" nil t)
(+xfu--def-browse-in! private +private-config-path)

;;;###autoload (autoload '+xfu/find-in-snippets "autoload/xfu" nil t)
(+xfu--def-find-in! snippets +xfu-snippets-dir)
;; NOTE No need for a browse-snippets variant, use `yas-visit-snippet-file'

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
(defmacro +default--def-browse-in! (name dir &optional prefix)
  (let ((prefix (or prefix (cdr (doom-module-from-path (or load-file-name byte-compile-current-file))))))
    `(defun ,(intern (format "+%s/browse-%s" prefix name)) ()
       (interactive)
       (doom-project-browse ,dir))))

;;;###autoload
(defmacro +default--def-find-in! (name dir &optional prefix)
  (let ((prefix (or prefix (cdr (doom-module-from-path (or load-file-name byte-compile-current-file))))))
    `(defun ,(intern (format "+%s/find-in-%s" prefix name)) ()
       (interactive)
       (doom-project-find-file ,dir))))

