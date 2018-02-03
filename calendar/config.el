;;; private/calendar/config.el -*- lexical-binding: t; -*-
(defvar org-gcal-secret-file "~/.emacs.d/modules/private/org/secret.el")
(defvar +calendar-open-calendar-function '+calendar/open-calendar)
(def-package! calfw
  :commands (cfw:open-calendar-buffer)
  :config

  ;; better frame for calendar
  (setq
   cfw:render-line-breaker 'cfw:render-line-breaker-none
   cfw:face-item-separator-color nil
   cfw:fchar-junction ?╋
   cfw:fchar-vertical-line ?┃
   cfw:fchar-horizontal-line ?━
   cfw:fchar-left-junction ?┣
   cfw:fchar-right-junction ?┫
   cfw:fchar-top-junction ?┯
   cfw:fchar-top-left-corner ?┏
   cfw:fchar-top-right-corner ?┓)


  (defun cfw:render-button (title command &optional state)
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
  (map! :map cfw:calendar-mode-map
        "q" #'+calendar/quit)
  (add-hook! 'cfw:calendar-mode-hook (solaire-mode +1)
    (doom-hide-modeline-mode)))

(def-package! calfw-org
  :commands (cfw:open-org-calendar
             cfw:org-create-source
             cfw:open-org-calendar-withkevin
             my-open-calendar))

(def-package! org-gcal
  :commands (org-gcal-sync
             org-gcal-fetch
             org-gcal-post-at-point
             org-gcal-delete-at-point)
  :init
  (setq-default org-gcal-dir (concat doom-etc-dir "org-gcal/"))
  :config
  (load-file org-gcal-secret-file)
  ;; hack to avoid the deferred.el error
  (defun org-gcal--notify (title mes)
    (message "org-gcal::%s - %s" title mes))
  (defun org-gcal-post-at-point (&optional skip-import)
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
  )

