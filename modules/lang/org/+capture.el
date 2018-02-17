;;; lang/org/+capture.el -*- lexical-binding: t; -*-

(add-hook 'org-load-hook #'+org|init-capture)

(defvar +org-default-todo-file "inbox.org"
  "TODO")

(defvar +org-default-notes-file "inbox.org"
  "TODO")

(defvar org-capture-templates
  `(("GSA" "General Skim Annotation" entry
     (file+function (lambda () (buffer-file-name)) +org-move-point-to-heading)
     "* %^{Logging for...} :skim_annotation:read:
:PROPERTIES:
:Created: %U
:SKIM_NOTE: %(+reference/skim-get-annotation)
:SKIM_PAGE: %(+reference/get-skim-page-number)
:END:
%i
%?")
    ("SA" "Skim Annotation" entry
     (file+function org-ref-bibliography-notes +reference/org-move-point-to-capture-skim-annotation)
     "* %^{Logging for...} :skim_annotation:read:literature:
:PROPERTIES:
:Created: %U
:CITE: cite:%(+reference/skim-get-bibtex-key)
:SKIM_NOTE: %(+reference/skim-get-annotation)
:SKIM_PAGE: %(+reference/get-skim-page-number)
:END:
%i
%?")
    ("t" "Todo" entry
     (file "~/Dropbox/org/inbox.org")
     "* %^{Logging for...}
:PROPERTIES:
:Created: %U
:END:
%i
%?" )
    ("tl" "Todo with link" entry
     (file "~/Dropbox/org/inbox.org")
     "* %^{Logging for...}
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?" )
    ("ic" "Idea from Chrome" entry
     (file "~/Dropbox/org/idea.org")
     "* %^{Logging for...} :idea:
:PROPERTIES:
:Created: %U
:Linked: %(org-as-mac-chrome-get-frontmost-url)
:END:
%i
%?" )
    ("c" "Calendar" entry
     (file "~/Dropbox/org/cal/cal.org")
     "* %^{Logging for...}
%^{LOCATION}p
%^T
" )
    ("j" "Journal" entry
     (function +org-move-point-to-heading)
     "* %^{Logging for...} :logs:
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?"  :clock-in t  )
    ("M" "Meeting" entry
     (file+olp+datetree "~/Dropbox/org/meeting.org")
     "* %^{Logging for...} :logs:communication:
%^{Effort}p
%^T

- Things to discuss:

%i
%?"  :clock-in t  )
    ("m" "Meeting Minutes" entry
     (function +org-move-point-to-heading)
     "* %^{Logging for...} :logs:
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?"  :clock-in t  )
    ("u" "Write-up" entry
     (function +org-move-point-to-heading)
     "* %^{Logging for...} :writeup:
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?"  :clock-in t  )
    ("i" "Idea" entry
     (file "~/Dropbox/org/idea.org")
     "* %A :idea:
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?")
    ("dr" "Daily Review" entry
     (file+olp+datetree "~/Dropbox/org/review.org")
     "* %^{Review} :review:daily:
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%?"  )
    ("wr" "Week Review" entry
     (file+olp+datetree "~/Dropbox/org/review.org")
     "* %^{Review for...|Mood|Research|Learn|Entertainment|Life} :review:week:%\\1:
:PROPERTIES:
:Created: %U
:END:
%?"  )
    ("mr" "Month Review" entry
     (file+olp+datetree "~/Dropbox/org/review.org")
     "* %^{Review for...|Mood|Research|Learn|Entertainment|Life} :review:month:%\\1:
:PROPERTIES:
:Created: %U
:END:
%?"  )
    ))

(defun +org|init-capture ()
  (defvaralias 'org-default-notes-file '+org-default-notes-file)

  (setq org-default-notes-file (expand-file-name +org-default-notes-file +org-dir))

  (add-hook 'org-capture-prepare-finalize-hook #'counsel-org-tag)
  (add-hook 'org-capture-after-finalize-hook #'org-gcal-sync)
  (add-hook 'org-capture-after-finalize-hook #'+org-capture|cleanup-frame)

  (defun +org-move-point-to-heading ()
    (cond ((org-at-heading-p) (org-beginning-of-line))
          (t (org-previous-visible-heading 1))))

  (when (featurep! :feature evil)
    (add-hook 'org-capture-mode-hook #'evil-insert-state))

  (when (featurep! :ui doom-dashboard)
    (add-hook '+doom-dashboard-inhibit-functions #'+org-capture-frame-p)))
