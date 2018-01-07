;; private/xfu/autoload/xfu.el -*- lexical-binding: t; -*-
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


;;;###autoload (autoload '+xfu/browse-project "private/xfu/autoload/xfu" nil t)
(+xfu--def-browse-in! project (doom-project-root))

;;;###autoload (autoload '+xfu/find-in-templates "private/xfu/autoload/xfu" nil t)
(+xfu--def-find-in!   templates +file-templates-dir)
;;;###autoload (autoload '+xfu/browse-templates "private/xfu/autoload/xfu" nil t)
(+xfu--def-browse-in! templates +file-templates-dir)

;;;###autoload (autoload '+xfu/find-in-emacsd "private/xfu/autoload/xfu" nil t)
(+xfu--def-find-in!   emacsd doom-emacs-dir)
;;;###autoload (autoload '+xfu/browse-emacsd "private/xfu/autoload/xfu" nil t)
(+xfu--def-browse-in! emacsd doom-emacs-dir)

;;;###autoload (autoload '+xfu/find-in-notes "private/xfu/autoload/xfu" nil t)
(+xfu--def-find-in!   notes +org-dir)
;;;###autoload (autoload '+xfu/browse-notes "private/xfu/autoload/xfu" nil t)
(+xfu--def-browse-in! notes +org-dir)

;;;###autoload (autoload '+xfu/find-in-snippets "private/xfu/autoload/xfu" nil t)
(+xfu--def-find-in! snippets +xfu-snippets-dir)
;; NOTE No need for a browse-snippets variant, use `yas-visit-snippet-file'

