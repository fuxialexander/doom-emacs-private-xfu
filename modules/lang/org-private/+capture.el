;;; lang/org-private/+capture.el -*- lexical-binding: t; -*-


(add-hook 'org-load-hook #'+org-private|init-capture t)

(setq +org-default-notes-file "inbox.org"
      +org-default-todo-file "inbox.org")

(setq org-capture-templates
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
:Linked: %(org-mac-chrome-get-frontmost-url)
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
%?" :time-prompt t)
   ("wr" "Week Review" entry
    (file+olp+datetree "~/Dropbox/org/review.org")
    "* %^{Review for...|Mood|Research|Learn|Entertainment|Life} :review:week:%\\1:
:PROPERTIES:
:Created: %U
:END:
%?" :time-prompt t)
   ("mr" "Month Review" entry
    (file+olp+datetree "~/Dropbox/org/review.org")
    "* %^{Review for...|Mood|Research|Learn|Entertainment|Life} :review:month:%\\1:
:PROPERTIES:
:Created: %U
:END:
%?" :time-prompt t))
 +org-capture-window-params
 `((name . "org-capture")
   (fullscreen . fullwidth)
   (height . 40)
   (vertical-scroll-bars . nil)
   (horizontal-scroll-bars . nil)
   (left-fringe . 0)
   (right-fringe . 0)
   (menu-bar-lines . 0)
   (tool-bar-lines . 0)
   (undecorated . t)
   (no-special-glyphs . t)
   (ns-appearance . nil)
   (window-system . ,(cond (IS-MAC 'ns)
                           (IS-LINUX 'x)
                           (t 'w32)))
   ,(if IS-LINUX '(display . ":0"))))

(defun +org-private|init-capture ()
  (add-hook 'org-capture-prepare-finalize-hook #'counsel-org-tag)
  (if (featurep! calendar)
      (add-hook 'org-capture-after-finalize-hook #'org-gcal-sync)))
