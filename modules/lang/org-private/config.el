;;; lang/org-private/config.el -*- lexical-binding: t; -*-

(after! org
  (add-hook 'org-metareturn-hook '+org/insert-go-eol)
  (add-hook 'org-ctrl-c-ctrl-c-hook '+org-private/*org-ctrl-c-ctrl-c-counsel-org-tag)
  (add-hook 'org-mode-hook (lambda () (add-hook 'before-save-hook '+org-private/org-add-ids-to-headlines-in-file nil 'local)))
  (add-hook 'org-metareturn-hook '+org/insert-item-with-ts)
  (advice-add 'org-eldoc-get-breadcrumb :filter-return #'*org-eldoc-get-timestamp)
  (advice-add 'org-format-outline-path :filter-return #'*org-format-outline-path-normalize)
  (advice-add 'org-set-font-lock-defaults :override #'*org-set-font-lock-defaults)
  (defface org-deadline-custom '((t (:inherit 'default))) "org-deadline" :group 'org)
  (advice-add 'org-shiftcontrolup :override #'*org/shiftcontrolup)
  (advice-add 'org-shiftcontroldown :override #'*org/shiftcontroldown)
;; * UI
  (defface org-closed-custom-braket '((t (:inherit 'default))) "org-close" :group 'org)
  (setq org-adapt-indentation nil
        org-M-RET-may-split-line '((default . nil))
        org-export-use-babel nil
        org-blank-before-new-entry '((heading . t) (plain-list-item . nil))
        org-clock-clocktable-default-properties '(:maxlevel 3 :scope agenda :tags "-COMMENT")
        org-clocktable-defaults '(:maxlevel 3 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 t :fileskip0 t :tags "-COMMENT" :emphasize nil :link nil :narrow 40! :indent t :formula nil :timestamp nil :level nil :tcolumns nil :formatter nil)
        org-columns-default-format "%45ITEM %TODO %SCHEDULED %DEADLINE %3PRIORITY %TAGS %CLOCKSUM %EFFORT %BUDGET_WEEK %BUDGET_MONTH %BUDGET_QUARTER %BUDGET_YEAR"
        org-complete-tags-always-offer-all-agenda-tags t
        org-cycle-include-plain-lists t
        org-cycle-separator-lines 1
        org-enforce-todo-dependencies t
        org-bullets-bullet-list '("◉")
        org-ellipsis " >"
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-footnote-auto-label 'plain
        org-global-properties '(("Effort_ALL" . "0 0:05 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00 9:00 10:00 11:00 12:00 13:00 14:00 15:00 16:00 17:00 18:00 19:00 20:00"))
        org-hidden-keywords nil
        org-hide-block-startup t
        org-highest-priority ?A
        org-insert-heading-respect-content t
        org-id-link-to-org-use-id t
        org-id-locations-file (concat org-directory ".org-id-locations")
        org-id-track-globally t
        org-image-actual-width nil
        org-imenu-depth 8
        org-indent-indentation-per-level 2
        org-indent-mode-turns-on-hiding-stars t
        org-list-description-max-indent 4
        org-log-done 'time
        org-log-into-drawer t
        org-log-note-clock-out t
        org-lowest-priority ?F
        org-modules '(ol-bibtex
                      ol-info
                      org-protocol
                      org-mac-link
                      ol-w3m
                      ol-bibtex
                      ol-eww)
        org-outline-path-complete-in-steps nil
        org-pretty-entities nil
        org-pretty-entities-include-sub-superscripts t
        ;; org-priority-faces
        ;; `((?A . ,(face-foreground 'error))
        ;;   (?B . ,(face-foreground 'warning))
        ;;   (?C . ,(face-foreground 'success)))
        org-publish-timestamp-directory (concat org-directory ".org-timestamps/")
        org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9))
        org-refile-use-outline-path 'file
        org-startup-folded t
        org-startup-indented t
        org-startup-with-inline-images nil
        org-tags-column 0
        org-todo-keyword-faces
        '(("TODO" . org-todo-keyword-todo)
          ("WANT" . org-todo-keyword-want)
          ("DONE" . org-todo-keyword-done)
          ("WAIT" . org-todo-keyword-wait)
          ("KILL" . org-todo-keyword-kill)
          ("OUTD" . org-todo-keyword-outd))
        org-todo-keywords
        '((sequence "TODO(t!)" "WAIT(w@/@)" "WANT(h!/@)" "|" "DONE(d!/@)" "OUTD(o@/@)" "KILL(k@/@)"))
        org-treat-insert-todo-heading-as-state-change t
        org-use-fast-tag-selection nil
        org-use-fast-todo-selection t
        outline-blank-line t
        org-file-apps
        `(("pdf" . emacs)
          ("\\.x?html?\\'" . default)
          (auto-mode . emacs)
          (directory . emacs)
          (t . ,(cond (IS-MAC "open \"%s\"")
                      (IS-LINUX "xdg-open \"%s\"")))))
  (setq org-ts-regexp-both-braket "\\([[<]\\)\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ?[^]\n>]*?\\)\\([]>]\\)")
  (defface org-deadline-custom-braket '((t (:inherit 'default))) "org-deadline" :group 'org)
  (defface org-scheduled-custom-braket '((t (:inherit 'default))) "org-schedule" :group 'org)
  (defface org-priority-hide '((t ())) "org-priority-hide" :group 'org)
  (defface org-scheduled-custom '((t (:inherit 'default))) "org-schedule" :group 'org)
  (defface org-closed-custom '((t (:inherit 'default))) "org-close" :group 'org)
  (defface org-todo-keyword-done '((t ())) "org-done" :group 'org)
  (defface org-todo-keyword-want '((t ())) "org-want" :group 'org)
  (defface org-todo-keyword-outd '((t ())) "org-outd" :group 'org)
  (defface org-todo-keyword-wait '((t ())) "org-wait" :group 'org)
  (defface org-todo-keyword-todo '((t ())) "org-todo" :group 'org)
  (defface org-todo-keyword-kill '((t ())) "org-kill" :group 'org))

;; * Agenda
(after! org-agenda
  (setq org-agenda-block-separator ""
        org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3 :fileskip0 t :stepskip0 t :tags "-COMMENT")
        org-agenda-compact-blocks t
        org-agenda-files (ignore-errors (directory-files org-directory t "^\\(.*\\|ref\\)\\.org$" t))
        org-agenda-follow-indirect t
        ;; org-agenda-ignore-properties '(effort appt category)
        ;; org-agenda-use-tag-inheritance nil
        org-agenda-inhibit-startup t
        org-agenda-log-mode-items '(closed clock)
        org-agenda-overriding-header ""
        org-agenda-restore-windows-after-quit t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-deadline-prewarning-if-scheduled t
        org-agenda-skip-scheduled-if-done t
        org-agenda-start-with-log-mode t
        org-agenda-sticky nil
        org-agenda-tags-column 'auto)

  (org-super-agenda-mode)
  (defhydra +org@org-agenda-filter (:color pink :hint nil)
    "
_;_ tag      _h_ headline      _c_ category     _r_ regexp     _d_ remove    "
    (";" org-agenda-filter-by-tag)
    ("h" org-agenda-filter-by-top-headline)
    ("c" org-agenda-filter-by-category)
    ("r" org-agenda-filter-by-regexp)
    ("d" org-agenda-filter-remove-all)
    ("q" nil "cancel" :color blue))
  (set-popup-rule! "^\\*Org Agenda.*" :slot -1 :size 120 :side 'left :select t)
  ;; (set-evil-initial-state! 'org-agenda-mode 'normal)
  )
(use-package! org-super-agenda
  :commands (org-super-agenda-mode)
  :init (advice-add #'org-super-agenda-mode :around #'doom-shut-up-a)
  :config
  (setq org-super-agenda-groups
        '((:name "Log\n"
                 :log t)  ; Automatically named "Log"
          (:name "Schedule\n"
                 :time-grid t)
          (:name "Today\n"
                 :scheduled today)
          (:name "Due today\n"
                 :deadline today)
          (:name "Overdue\n"
                 :deadline past)
          (:name "Due soon\n"
                 :deadline future)
          (:name "Waiting\n"
                 :todo "WAIT"
                 :order 98)
          (:name "Scheduled earlier\n"
                 :scheduled past))))

;; * Export
(defvar +org-html-embed-image t
  "whether image is embeded as base64 in html")
(defvar +org-html-export-style-dir "~/.doom.d/modules/lang/org-private/org-html-head"
  "Directory that contains files to be embeded into org export html.")
(defvar +org-html-export-style-alist '("include.html"
                                       "bootstrap-toc.js"
                                       "bootstrap-toc.css"
                                       "org.js"
                                       "org.css")
  "a list of scripts to be included in org-html-head")
(after! ox-html
  (add-hook 'org-export-before-processing-hook #'+org/org-embed-header)
  (advice-add #'org-html--format-image :override #'+org*org-html--format-image)
  (advice-add 'org-html--tags :override #'+org-private/org-html--tags)
  (setq org-html-checkbox-type 'html
        org-html-mathjax-options
        '((path "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-AMS_SVG")
          (scale "100")
          (align "center")
          (font "TeX")
          (linebreaks "false")
          (autonumber "AMS")
          (indent "0em")
          (multlinewidth "85%")
          (tagindent ".8em")
          (tagside "right"))
        org-html-table-default-attributes
        '(:border "2"
                  :class "table table-striped table-sm table-bordered"
                  :cellspacing "0"
                  :cellpadding "6"
                  :rules "groups"
                  :frame "hsides")))
(after! org
  (setq org-export-backends '(ascii html latex md)
        org-export-async-debug t
        org-export-async-init-file (concat doom-private-dir "local/ox-init.el")
        org-publish-timestamp-directory (concat doom-cache-dir "/org-timestamps/")))
(after! ox
  (when (executable-find "pandoc")
    (require 'ox-pandoc)))
(use-package! htmlize
  :commands (htmlize-buffer
             htmlize-file
             htmlize-many-files
             htmlize-many-files-dired)
  :config
  (setq-default htmlize-pre-style t))

;; * Babel
(after! ob
  (setq +org-babel-mode-alist
        '((jupyter-r . ipython)
          (zsh . shell)
          (bash . shell)
          (sh . shell)))
  (advice-add #'org-edit-src-code :override #'+org-private*org-edit-src-code)
  (setq org-src-preserve-indentation nil
        org-list-description-max-indent 5
        org-edit-src-content-indentation 0)
  (after! ivy
    (ivy-add-actions '+org-private/get-name-src-block
                     '(("g" org-babel-goto-named-src-block "Goto")))))

;; * Capture
(after! org-capture
  (add-hook 'org-capture-prepare-finalize-hook #'counsel-org-tag)
  ;; (if (featurep! calendar)
  ;;     (add-hook 'org-capture-after-finalize-hook #'org-gcal-sync))
  (setq org-default-notes-file "inbox.org"
        +org-capture-todo-file "inbox.org"
        +org-capture-notes-file "inbox.org"
        org-capture-templates
        `(("GSA" "General Skim Annotation" entry
           (file+function (lambda () (buffer-file-name)) +org-move-point-to-heading)
           "* %^{Logging for...} :skim_annotation:read:
:PROPERTIES:
:Created: %U
:SKIM_NOTE: %(+org-reference-skim-get-annotation)
:SKIM_PAGE: %(+org-reference-get-skim-page-number)
:END:
%i
%?")
          ("SA" "Skim Annotation" entry
           (file+function org-ref-bibliography-notes +org-reference-org-move-point-to-capture-skim-annotation)
           "* %^{Logging for...} :skim_annotation:read:literature:
:PROPERTIES:
:Created: %U
:CITE: cite:%(+org-reference-skim-get-bibtex-key)
:SKIM_NOTE: %(+org-reference-skim-get-annotation)
:SKIM_PAGE: %(+org-reference-get-skim-page-number)
:END:
%i
%?")
          ("t" "Todo" entry
           (file ,(concat org-directory "inbox.org"))
           "* %^{Logging for...}
:PROPERTIES:
:Created: %U
:END:
%i
%?")
          ("tl" "Todo with link" entry
           (file ,(concat org-directory "inbox.org"))
           "* %^{Logging for...}
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?")
          ("ic" "Idea from Chrome" entry
           (file ,(concat org-directory "idea.org"))
           "* %^{Logging for...} :idea:
:PROPERTIES:
:Created: %U
:Linked: %(org-mac-chrome-get-frontmost-url)
:END:
%i
%?")
          ("j" "Journal" entry
           #'+org-move-point-to-heading
           "* %^{Logging for...} :logs:
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?" :clock-in t)
          ("M" "Meeting" entry
           (file+olp+datetree ,(concat org-directory "meeting.org"))
           "* %^{Logging for...} :logs:communication:
%^{Effort}p
%i
%?" :clock-in t)
          ("m" "Meeting Minutes" entry
           #'+org-move-point-to-heading
           "* %^{Logging for...} :logs:
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?" :clock-in t)
          ("u" "Write-up" entry
           #'+org-move-point-to-heading
           "* %^{Logging for...} :writeup:
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?" :clock-in t)
          ("i" "Idea" entry
           (file ,(concat org-directory "idea.org"))
           "* %A :idea:
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?")
          ("drl" "Daily Review with link" entry
           (file+olp+datetree ,(concat org-directory "review.org"))
           "* %^{Review} :review:daily:
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%?" :time-prompt t)
          ("dr" "Daily Review" entry
           (file+olp+datetree ,(concat org-directory "review.org"))
           "* %^{Review} :review:daily:
:PROPERTIES:
:Created: %U
:END:
%?" :time-prompt t)
          ("wr" "Week Review" entry
           (file+olp+datetree ,(concat org-directory "review.org"))
           "* %^{Review for...|Mood|Research|Learn|Entertainment|Life} :review:week:%\\1:
:PROPERTIES:
:Created: %U
:END:
%?" :time-prompt t)
          ("mr" "Month Review" entry
           (file+olp+datetree ,(concat org-directory "review.org"))
           "* %^{Review for...|Mood|Research|Learn|Entertainment|Life} :review:month:%\\1:
:PROPERTIES:
:Created: %U
:END:
%?" :time-prompt t))))
;; * Other Plugins
(use-package! org-clock-convenience
  :commands (org-clock-convenience-timestamp-up
             org-clock-convenience-timestamp-down
             org-clock-convenience-fill-gap
             org-clock-convenience-fill-gap-both))
(use-package! org-clock-budget
  :commands (org-clock-budget-report)
  :init
  (defun my-buffer-face-mode-org-clock-budget ()
    "Sets a fixed width (monospace) font in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "Iosevka" :height 1.0))
    (buffer-face-mode)
    (setq-local line-spacing nil))
  :config
  (map! :map org-clock-budget-report-mode-map
        :nm "h" #'org-shifttab
        :nm "l" #'org-cycle
        :nm "e" #'org-clock-budget-report
        :nm "s" #'org-clock-budget-report-sort
        :nm "d" #'org-clock-budget-remove-budget
        :nm "q" #'quit-window)
  (add-hook! 'org-clock-budget-report-mode-hook
    (toggle-truncate-lines 1)
    (my-buffer-face-mode-org-clock-budget)))
(use-package! webkit-katex-render
  :when (featurep 'xwidget-internal)
  :commands (webkit-katex-render-mode)
  :config
  (setq webkit-katex-render--background-color (doom-color 'bg)))
(use-package! cdlatex
  :commands (org-cdlatex-mode
             cdlatex-mode
             turn-on-cdlatex
             turn-on-org-cdlatex)
  :init
  (setq cdlatex-math-modify-alist '((?B "\\mathbb" nil t nil nil))))
(use-package! org-brain
  :after org
  :commands (org-brain-visualize)
  :init
  (setq org-brain-path (concat org-directory "brain/"))
  (after! evil-snipe
    (push 'org-brain-visualize-mode evil-snipe-disabled-modes))
  ;; (add-hook 'org-agenda-mode-hook #'(lambda () (evil-vimish-fold-mode -1)))
  (set-evil-initial-state! 'org-brain-visualize-mode 'normal)
  :config
  (set-popup-rule! "^\\*org-brain\\*$" :vslot -1 :size 0.3 :side 'left :select t)
  (defun org-brain-set-tags (entry)
    "Use `org-set-tags' on headline ENTRY.
If run interactively, get ENTRY from context."
    (interactive (list (org-brain-entry-at-pt)))
    (when (org-brain-filep entry)
      (error "Can only set tags on headline entries"))
    (org-with-point-at (org-brain-entry-marker entry)
      (counsel-org-tag)
      (save-buffer))
    (org-brain--revert-if-visualizing))
  (setq org-brain-visualize-default-choices 'all
        org-brain-file-entries-use-title nil
        org-brain-title-max-length 30)
  (map!
   (:map org-brain-visualize-mode-map
     :n "a" #'org-brain-visualize-attach
     :n "b" #'org-brain-visualize-back
     :n "c" #'org-brain-add-child
     :n "C" #'org-brain-remove-child
     :n "p" #'org-brain-add-parent
     :n "P" #'org-brain-remove-parent
     :n "f" #'org-brain-add-friendship
     :n "F" #'org-brain-remove-friendship
     :n "d" #'org-brain-delete-entry
     :n "g" #'revert-buffer
     :n "_" #'org-brain-new-child
     :n ";" #'org-brain-set-tags
     :n "j" #'forward-button
     :n "k" #'backward-button
     :n "l" #'org-brain-add-resource
     :n "L" #'org-brain-visualize-paste-resource
     :n "t" #'org-brain-set-title
     :n "$" #'org-brain-pin
     :n "o" #'ace-link-woman
     :n "q" #'org-brain-visualize-quit
     :n "r" #'org-brain-visualize-random
     :n "R" #'org-brain-visualize-wander
     :n "s" #'org-brain-visualize
     :n "S" #'org-brain-goto
     :n [tab] #'org-brain-goto-current
     :n "m" #'org-brain-visualize-mind-map
     :n "[" #'org-brain-visualize-add-grandchild
     :n "]" #'org-brain-visualize-remove-grandchild
     :n "{" #'org-brain-visualize-add-grandparent
     :n "}" #'org-brain-visualize-remove-grandparent
     )))
