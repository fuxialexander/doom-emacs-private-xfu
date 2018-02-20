;;; modules/app/calendar/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun =calendar ()
  "Activate (or switch to) `calendar' in its workspace."
  (interactive)
  (unless (featurep! :feature workspaces)
    (user-error ":feature workspaces is required, but disabled"))
  (+workspace-switch "Calendar" t)
  (if-let* ((buf (cl-find-if (lambda (it) (string-match-p "^\\*cfw" (buffer-name (window-buffer it))))
                             (doom-visible-windows))))
      (select-window (get-buffer-window buf))
    (call-interactively +calendar-open-calendar-function))
  (+workspace/display))

;;;###autoload
(defun +calendar/quit ()
  (interactive)
  (+workspace/delete "Calendar"))

;;;###autoload
(defun +calendar/open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source (doom-color 'fg))  ; orgmode source
    )))

;;;###autoload
(defun +calendar*cfw:render-button (title command &optional state)
    "render-button
 TITLE
 COMMAND
 STATE"
    (let ((text (concat " " title " "))
          (keymap (make-sparse-keymap)))
      (cfw:rt text (if state 'cfw:face-toolbar-button-on
                     'cfw:face-toolbar-button-off))
      (define-key keymap [mouse-1] command)
      (cfw:tp text 'keymap keymap)
      (cfw:tp text 'mouse-face 'highlight)
      text))

;;;###autoload
(defun +calendar*org-gcal-post-at-point (&optional skip-import)
    (interactive)
    (org-gcal--ensure-token)
    (save-excursion
      (end-of-line)
      (org-back-to-heading)
      (let* ((skip-import skip-import)
             (elem (org-element-headline-parser (point-max) t))
             (tobj (progn (re-search-forward "<[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
                                             (save-excursion (outline-next-heading) (point)))
                          (goto-char (match-beginning 0))
                          (org-element-timestamp-parser)))
             (smry (org-element-property :title elem))
             (loc  (org-element-property :LOCATION elem))
             (id  (org-element-property :ID elem))
             (start (org-gcal--format-org2iso
                     (plist-get (cadr tobj) :year-start)
                     (plist-get (cadr tobj) :month-start)
                     (plist-get (cadr tobj) :day-start)
                     (plist-get (cadr tobj) :hour-start)
                     (plist-get (cadr tobj) :minute-start)
                     (when (plist-get (cadr tobj) :hour-start)
                       t)))
             (end (org-gcal--format-org2iso
                   (plist-get (cadr tobj) :year-end)
                   (plist-get (cadr tobj) :month-end)
                   (plist-get (cadr tobj) :day-end)
                   (plist-get (cadr tobj) :hour-end)
                   (plist-get (cadr tobj) :minute-end)
                   (when (plist-get (cadr tobj) :hour-start)
                     t)))
             (desc  (if (plist-get (cadr elem) :contents-begin)
                        (replace-regexp-in-string
                         "\\(?: *<[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].*?>\\)\n?\n?" "" (replace-regexp-in-string
                          " *:PROPERTIES:\n ?\\(.*\\(?:\n.*\\)*?\\) ?:END:\n?\n?" ""
                          (buffer-substring-no-properties
                           (plist-get (cadr elem) :contents-begin)
                           (plist-get (cadr elem) :contents-end)))) "")))
        (org-gcal--post-event start end smry loc desc id nil skip-import))))
