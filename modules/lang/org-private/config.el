;;; lang/org-private/config.el -*- lexical-binding: t; -*-

;; Sub-modules
(if (featurep! +todo)    (load! "+todo"))
(if (featurep! +babel)   (load! "+babel"))
(if (featurep! +ipython) (load! "+ipython"))
(if (featurep! +latex)   (load! "+latex"))
(if (featurep! +capture) (load! "+capture"))
(if (featurep! +export)  (load! "+export"))

;; TODO (if (featurep! +publish) (load! +publish))




;;
;; Plugins
;;

(def-package! org-brain
  :after org
  :commands (org-brain-visualize)
  :init
  (setq org-brain-path "~/Dropbox/org/brain")
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

;; (def-package! org-web-tools
;;   :after org)

(def-package! evil-org
  :when (featurep! :feature evil)
  :hook (org-mode . evil-org-mode)
  :hook (org-load . evil-org-set-key-theme)
  :init
  (setq evil-org-want-hybrid-shift t
        evil-org-use-additional-insert nil
        evil-org-key-theme '(navigation
                             shift
                             todo
                             additional
                             operators
                             insert
                             textobjects))
  (add-hook 'org-load-hook #'+org-private|setup-keybinds t)
  (advice-add 'evil-org-open-below :override #'+org-private*evil-org-open-below))

;;
;; Bootstrap
;;

(add-hook 'org-load-hook #'+org-private|setup-ui t)
(add-hook 'org-load-hook #'+org-private|setup-agenda t)
(add-hook 'org-load-hook #'+org-private|setup-overrides t)




(remove-hook! 'org-mode-hook #'(visual-line-mode
                                toc-org-enable))

;; (add-hook 'org-mode-hook #'+org-private|setup-editing t)
(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'org-mode-hook #'eldoc-mode t)

;;
;; `org-load' hooks
;;

(defun +org-private|setup-agenda ()
  (setq org-agenda-block-separator ""
        org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 3 :fileskip0 t :stepskip0 t :tags "-COMMENT"))
        org-agenda-compact-blocks t
        org-agenda-dim-blocked-tasks nil
        org-agenda-files (ignore-errors (directory-files org-directory t "^\\(.*\\|ref\\)\\.org$" t))
        org-agenda-follow-indirect t
        org-agenda-ignore-properties '(effort appt category)
        org-agenda-inhibit-startup t
        org-agenda-inhibit-startup t
        org-agenda-log-mode-items '(closed clock)
        org-agenda-overriding-header ""
        org-agenda-restore-windows-after-quit t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-deadline-prewarning-if-scheduled t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-unavailable-files t
        org-agenda-sorting-strategy '((agenda time-up priority-down category-keep)
                                      (todo   priority-down category-keep)
                                      (tags   priority-down category-keep)
                                      (search category-keep))
        org-agenda-span 'day
        org-agenda-start-with-log-mode t
        org-agenda-sticky nil
        org-agenda-tags-column 'auto
        org-agenda-use-tag-inheritance nil))

(defun +org-private|setup-ui ()
  "Configures the UI for `org-mode'."
  (defface org-todo-keyword-todo '((t ())) "org-todo" :group 'org)
  (defface org-todo-keyword-kill '((t ())) "org-kill" :group 'org)
  (defface org-todo-keyword-outd '((t ())) "org-outd" :group 'org)
  (defface org-todo-keyword-wait '((t ())) "org-wait" :group 'org)
  (defface org-todo-keyword-done '((t ())) "org-done" :group 'org)
  (defface org-todo-keyword-want '((t ())) "org-want" :group 'org)
  (defface org-priority-hide '((t ())) "org-priority-hide" :group 'org)

  ;; (advice-remove #'org-src-switch-to-buffer #'+popup*org-src-pop-to-buffer)
  ;; (set-popup-rule! "^\\*Org Src" :size 100 :side 'bottom :slot -1 :height 0.6 :select t)
  (set-popup-rule! "^CAPTURE.*\\.org$" :side 'bottom :size 0.4 :select t)

  ;; setup customized font lock
  (setq org-ts-regexp-both-braket "\\([[<]\\)\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ?[^]\n>]*?\\)\\([]>]\\)")
  (defun *org-set-font-lock-defaults ()
    "Set font lock defaults for the current buffer."
    (let* ((em org-fontify-emphasized-text)
           (lk org-highlight-links)
           (org-font-lock-extra-keywords
            (list
             ;; Call the hook
             '(org-font-lock-hook)
             ;; Headlines
             `(,(if org-fontify-whole-heading-line
                    "^\\(\\**\\)\\(\\* \\)\\(.*\n?\\)"
                  "^\\(\\**\\)\\(\\* \\)\\(.*\\)")
               (1 (org-get-level-face 1))
               (2 (org-get-level-face 2))
               (3 (org-get-level-face 3)))
             ;; Table lines
             '("^[ \t]*\\(\\(|\\|\\+-[-+]\\).*\\S-\\)"
               (1 'org-table t))
             ;; Table internals
             '("^[ \t]*|\\(?:.*?|\\)? *\\(:?=[^|\n]*\\)" (1 'org-formula t))
             '("^[ \t]*| *\\([#*]\\) *|" (1 'org-formula t))
             '("^[ \t]*|\\( *\\([$!_^/]\\) *|.*\\)|" (1 'org-formula t))
             '("| *\\(<[lrc]?[0-9]*>\\)" (1 'org-formula t))
             ;; Drawers
             '(org-fontify-drawers)
             ;; Properties
             (list org-property-re
                   '(1 'org-special-keyword t)
                   '(3 'org-property-value t))
             ;; Special keywords
             (list (concat "\\<\\(DEADLINE: \\)" org-ts-regexp-both-braket)
                   '(1 'org-deadline-custom t)
                   '(2 'org-deadline-custom-braket t)
                   '(3 'org-deadline-custom t)
                   '(4 'org-deadline-custom-braket t))
             (list (concat "\\<\\(SCHEDULED: \\)" org-ts-regexp-both-braket)
                   '(1 'org-scheduled-custom t)
                   '(2 'org-scheduled-custom-braket t)
                   '(3 'org-scheduled-custom t)
                   '(4 'org-scheduled-custom-braket t))
             (list (concat "\\<\\(CLOSED: \\)" org-ts-regexp-both-braket)
                   '(1 'org-closed-custom t)
                   '(2 'org-closed-custom-braket t)
                   '(3 'org-closed-custom t)
                   '(4 'org-closed-custom-braket t))
             (list (concat "\\<" org-clock-string) '(0 'org-special-keyword prepend))
             ;; Link related fontification.
             '(org-activate-links)
             (when (memq 'tag lk) '(org-activate-tags (1 'org-tag prepend)))
             (when (memq 'radio lk) '(org-activate-target-links (1 'org-link t)))
             (when (memq 'date lk) '(org-activate-dates (0 'org-date append)))
             (when (memq 'footnote lk) '(org-activate-footnote-links))
             ;; Targets.
             (list org-any-target-regexp '(0 'org-target t))
             ;; Diary sexps.
             '("^&?%%(.*\\|<%%([^>\n]*?>" (0 'org-sexp-date t))
             ;; Macro
             '(org-fontify-macros)
             ;; TODO keyword
             (list (format org-heading-keyword-regexp-format
                           org-todo-regexp)
                   '(2 (org-get-todo-face 2) t))
             ;; DONE
             (if org-fontify-done-headline
                 (list (format org-heading-keyword-regexp-format
                               (concat
                                "\\(?:"
                                (mapconcat 'regexp-quote org-done-keywords "\\|")
                                "\\)"))
                       '(2 'org-headline-done t))
               nil)
             ;; Priorities
             '(org-font-lock-add-priority-faces)
             ;; Tags
             '(org-font-lock-add-tag-faces)
             ;; Tags groups
             (when (and org-group-tags org-tag-groups-alist)
               (list (concat org-outline-regexp-bol ".+\\(:"
                             (regexp-opt (mapcar 'car org-tag-groups-alist))
                             ":\\).*$")
                     '(1 'org-tag-group prepend)))

             ;; Emphasis
             (when em '(org-do-emphasis-faces))
             ;; Checkboxes
             '("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\(\\[[- X]\\]\\)"
               1 'org-checkbox prepend)
             (when (cdr (assq 'checkbox org-list-automatic-rules))
               '("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                 (0 (org-get-checkbox-statistics-face) t)))
             ;; Description list items
             '("^[ \t]*[-+*][ \t]+\\(.*?[ \t]+::\\)\\([ \t]+\\|$\\)"
               1 'org-list-dt prepend)
             ;; ARCHIVEd headings
             (list (concat
                    org-outline-regexp-bol
                    "\\(.*:" org-archive-tag ":.*\\)")
                   '(1 'org-archived prepend))
             ;; Specials
             '(org-do-latex-and-related)
             '(org-fontify-entities)
             '(org-raise-scripts)
             ;; Code
             '(org-activate-code (1 'org-code t))
             ;; COMMENT
             (list (format
                    "^\\*+\\(?: +%s\\)?\\(?: +\\[#[A-Z0-9]\\]\\)? +\\(?9:%s\\)\\(?: \\|$\\)"
                    org-todo-regexp
                    org-comment-string)
                   '(9 'org-special-keyword t))
             ;; Blocks and meta lines
             '(org-fontify-meta-lines-and-blocks))))
      (setq org-font-lock-extra-keywords (delq nil org-font-lock-extra-keywords))
      (run-hooks 'org-font-lock-set-keywords-hook)
      ;; Now set the full font-lock-keywords
      (setq-local org-font-lock-keywords org-font-lock-extra-keywords)
      (setq-local font-lock-defaults
                  '(org-font-lock-keywords t nil nil backward-paragraph))
      (kill-local-variable 'font-lock-keywords)
      nil))

  (defun org-font-lock-add-priority-faces (limit)
        "Add the special priority faces."
        (while (re-search-forward "^\\*+ .*?\\(\\[\\(#\\(.\\)\\)\\]\\)" limit t)
          (add-face-text-property
           (match-beginning 1) (match-end 1)
           'org-priority-hide)
          (add-face-text-property
           (match-beginning 2) (match-end 2)
           (org-get-priority-face (string-to-char (match-string 3))))))

  (advice-add 'org-set-font-lock-defaults :override #'*org-set-font-lock-defaults)
  (defface org-deadline-custom '((t (:inherit 'default))) "org-deadline" :group 'org)
  (defface org-scheduled-custom '((t (:inherit 'default))) "org-schedule" :group 'org)
  (defface org-closed-custom '((t (:inherit 'default))) "org-close" :group 'org)
  (defface org-deadline-custom-braket '((t (:inherit 'default))) "org-deadline" :group 'org)
  (defface org-scheduled-custom-braket '((t (:inherit 'default))) "org-schedule" :group 'org)
  (defface org-closed-custom-braket '((t (:inherit 'default))) "org-close" :group 'org)
  (setq org-adapt-indentation nil
        org-M-RET-may-split-line '((default . nil))
        org-export-babel-evaluate nil
        org-blank-before-new-entry '((heading . t) (plain-list-item . nil))
        org-clock-clocktable-default-properties '(:maxlevel 3 :scope agenda :tags "-COMMENT")
        org-clocktable-defaults '(:maxlevel 3 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 t :fileskip0 t :tags "-COMMENT" :emphasize nil :link nil :narrow 40! :indent t :formula nil :timestamp nil :level nil :tcolumns nil :formatter nil)
        org-columns-default-format "%45ITEM %TODO %SCHEDULED %DEADLINE %3PRIORITY %TAGS %CLOCKSUM %EFFORT %BUDGET_WEEK %BUDGET_MONTH %BUDGET_QUARTER %BUDGET_YEAR"
        org-complete-tags-always-offer-all-agenda-tags t
        org-cycle-include-plain-lists t
        org-cycle-separator-lines 1
        org-mac-Skim-highlight-selection-p t
        org-enforce-todo-dependencies t
        org-ellipsis "⤵"
        org-entities-user
        '(("flat" "\\flat" nil "" "" "266D" "♭")
          ("sharp" "\\sharp" nil "" "" "266F" "♯"))
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-footnote-auto-label 'plain
        org-global-properties '(("Effort_ALL" . "0 0:05 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00 9:00 10:00 11:00 12:00 13:00 14:00 15:00 16:00 17:00 18:00 19:00 20:00"))
        org-hidden-keywords nil
        org-hide-block-startup t
        org-hide-emphasis-markers nil
        org-hide-leading-stars nil
        org-hide-leading-stars-before-indent-mode nil
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
        org-log-note-headings '((done . "%t: DONE")
                                (state . "%t: %-4S -> %-4s")
                                (note . "%t: NOTE")
                                (reschedule . "%t: %S -> RESCHEDULE")
                                (delschedule . "%t: %S -> DESCHEDULE")
                                (redeadline . "%t: %S -> REDEADLINE")
                                (deldeadline . "%t: %S -> DEDEADLINE")
                                (refile . "%t: REFILE")
                                (clock-out . ""))
        org-log-redeadline 'time
        org-log-reschedule 'time
        org-lowest-priority ?F
        org-modules '(org-bibtex org-info org-protocol org-mac-link org-notmuch)
        org-outline-path-complete-in-steps nil
        org-pretty-entities nil

        org-pretty-entities-include-sub-superscripts t
        org-priority-faces
        `((?A . ,(face-foreground 'error))
          (?B . ,(face-foreground 'warning))
          (?C . ,(face-foreground 'success)))
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
        org-use-sub-superscripts '{}
        outline-blank-line t)
  ;; Update UI when theme is changed
  (add-hook 'doom-load-theme-hook #'+org-private|setup-ui)
  ;; (org-clock-persistence-insinuate)
  )

(defun +org-private|setup-keybinds ()
  (require 'evil-org)
  (add-hook 'org-tab-first-hook #'+org|cycle-only-current-subtree t)
  (advice-add #'org-return-indent :after #'+org*fix-newline-and-indent-in-src-blocks)


  (after! evil-org
    (map! (:map evil-org-mode-map
            ;; :i <S-tab> #'+org/dedent
            "M-o" #'org-open-at-point
            "M-i" #'org-insert-last-stored-link
            "M-I" #'org-insert-link
            "M-p" #'org-ref-ivy-insert-cite-link
            :nvime "C-j" (lambda! (org-next-visible-heading 1) (recenter))
            :nvime "C-k" (lambda! (org-previous-visible-heading 1) (recenter))
            :nv "M-j" nil
            :nv "M-k" nil
            :nv "M-l" nil
            :nv "M-h" nil

            :ni "<M-backspace>" #'org-babel-remove-result
            :ni "<M-return>" #'+org/work-on-heading
            :n "RET" #'+org/dwim-at-point
            :i "RET" #'org-return-indent
            :n [tab] #'org-cycle
            :n "M-t" nil
            :m "]v" #'org-next-block
            :m "[v" #'org-previous-block
            :m "]i" #'org-next-item
            :m "[i" #'org-previous-item
            :m "]h" #'org-next-visible-heading
            :m "[h" #'org-previous-visible-heading
            :m "_" #'evil-org-beginning-of-line
            :m "0" (λ! (let ((visual-line-mode)) (org-beginning-of-line)))
            :n "gQ" #'org-fill-paragraph
            :ni [M-return] #'org-meta-return
            :ni [S-M-return] (lambda! (+org/insert-go-eol)
                                      (call-interactively #'org-insert-todo-heading))
            (:localleader
              :n "," #'org-ctrl-c-ctrl-c
              :n "s" #'org-schedule
              :n "m" #'+org-toggle-math
              :n "b" #'+org-private@org-babel-hydra/body
              :n "c" #'org-columns
              :n "C" (lambda () (interactive) (let ((current-prefix-arg 2)) (call-interactively #'org-columns)))
              :n "L" #'+org/remove-link
              :n "d" #'org-deadline
              :n "'" #'org-edit-special
              :n "e" #'org-set-effort
              :n "t" #'org-todo
              :n "r" #'org-refile
              :n [tab] #'org-export-dispatch
              :n "E" #'org-clock-modify-effort-estimate
              :n "p" #'org-set-property
              :n "i" #'org-clock-in
              :n "o" #'org-clock-out
              :n "=" (λ! (call-interactively #'evil-append) (insert (+reference/skim-get-annotation)))
              :n "n" #'org-narrow-to-subtree
              :n "N" #'org-narrow-to-element
              :n "w" #'widen
              :n "$" #'wordnut-lookup-current-word
              :n "h" #'org-toggle-heading
              :n "A" #'org-archive-subtree
              :n "a" #'org-toggle-archive-tag))
          (:after org-agenda
            (:map org-agenda-mode-map
              ;; :nm <escape> #'org-agenda-Quit
              :nm "j" #'evil-next-line
              :nm "k" #'evil-previous-line
              :nm "J" #'org-clock-convenience-timestamp-down
              :nm "K" #'org-clock-convenience-timestamp-up
              :nm "M-j" #'org-agenda-later
              :nm "M-k" #'org-agenda-earlier
              :nm "M-o" #'org-clock-convenience-fill-gap
              :nm "M-e" #'org-clock-convenience-fill-gap-both
              ;; :nm "\\" #'ace-window
              :nm "t" #'org-agenda-todo
              :nm "p" #'org-set-property
              :nm "r" #'org-agenda-redo
              :nm "e" #'org-agenda-set-effort
              :nm "L" #'org-agenda-log-mode
              :nm "D" #'org-agenda-toggle-diary
              :nm "G" #'org-agenda-toggle-time-grid
              :nm ";" #'counsel-org-tag-agenda
              :nm "M-j" #'counsel-org-goto-all
              :nm "i" #'org-agenda-clock-in
              :nm "o" #'org-agenda-clock-out
              :nm [tab] #'org-agenda-goto
              :nm "C" #'org-agenda-capture
              :nm "m" #'org-agenda-bulk-mark
              :nm "u" #'org-agenda-bulk-unmark
              :nm "U" #'org-agenda-bulk-unmark-all
              :nm "f" #'+org@org-agenda-filter/body
              :nm "-" #'org-agenda-manipulate-query-subtract
              :nm "=" #'org-agenda-manipulate-query-add
              :nm "_" #'org-agenda-manipulate-query-subtract-re
              :nm "$" #'org-agenda-manipulate-query-add-re
              :nm "d" #'org-agenda-deadline
              :nm "q" #'org-agenda-quit
              :nm "s" #'org-agenda-schedule
              :nm "z" #'org-agenda-view-mode-dispatch
              :nm "S" #'org-save-all-org-buffers)
            (:map org-super-agenda-header-map
              "j" #'evil-next-line
              "k" #'evil-previous-line)))))

(defun +org-private|setup-overrides ()
  (after! org-html
    (defun +org-private/org-html--tags (tags info)
      "Format TAGS into HTML.
INFO is a plist containing export options."
      (when tags
        (format "\n<span class=\"tag\">%s</span>\n"
                (mapconcat
                 (lambda (tag)
                   (format "<span class=\"%s\">%s</span>"
                           (concat (plist-get info :html-tag-class-prefix)
                                   (org-html-fix-class-name tag))
                           tag))
                 tags " "))))
    (advice-add 'org-html--tags :override #'+org-private/org-html--tags))
  (setq org-file-apps
        `(("pdf" . default)
          ("\\.x?html?\\'" . default)
          (auto-mode . emacs)
          (directory . emacs)
          (t . ,(cond (IS-MAC "open \"%s\"")
                      (IS-LINUX "xdg-open \"%s\"")))))
  (defun +org-private/org-add-ids-to-headlines-in-file ()
    "Add CUSTOM_ID properties to all headlines in the current file"
    (interactive)
    (if (string-equal default-directory "~/Dropbox/org/")
        (save-excursion
        (widen)
        (goto-char (point-min))
        (org-map-entries 'org-id-get-create))))
  (add-hook 'org-mode-hook (lambda () (add-hook 'before-save-hook '+org-private/org-add-ids-to-headlines-in-file nil 'local)))
  (defun +org/insert-item-with-ts ()
    "When on org timestamp item insert org timestamp item with current time.
This holds only for inactive timestamps."
    (interactive)
    (when (save-excursion
            (let ((item-pos (org-in-item-p)))
              (when item-pos
                (goto-char item-pos)
                (org-list-at-regexp-after-bullet-p org-ts-regexp-inactive))))
      (let ((item-pos (org-in-item-p))
            (pos (point)))
        (assert item-pos)
        (goto-char item-pos)
        (let* ((struct (org-list-struct))
               (prevs (org-list-prevs-alist struct))
               (s (concat (with-temp-buffer
                            (org-insert-time-stamp nil t t)
                            (buffer-string)) " ")))
          (setq struct (org-list-insert-item pos struct prevs nil s))
          (org-list-write-struct struct (org-list-parents-alist struct))
          (looking-at org-list-full-item-re)
          (goto-char (match-end 0))
          (end-of-line)))
      t))

  (defun +org/insert-go-eol ()
    (when (bound-and-true-p evil-mode)
      (evil-insert 1))
    (end-of-line))
  (add-hook 'org-metareturn-hook '+org/insert-item-with-ts)
  (add-hook 'org-metareturn-hook '+org/insert-go-eol)

  (after! org-capture
    (defadvice org-capture-finalize
        (after org-capture-finalize-after activate)
      "Advise capture-finalize to close the frame"
      (if (or (equal "SA" (org-capture-get :key))
              (equal "GSA" (org-capture-get :key)))
          (do-applescript "tell application \"Skim\"\n    activate\nend tell")))
    (add-hook 'org-capture-prepare-finalize-hook
              #'(lambda () (if (or (equal "SA" (org-capture-get :key))
                                   (equal "GSA" (org-capture-get :key)))
                               (+reference/append-org-id-to-skim (org-id-get-create))))))
  ;; (after! elfeed-show
  ;;   (map! (:map elfeed-show-mode-map
  ;;           :n "b" #'+reference/elfeed-add)))
  (after! org-mac-link
    (org-link-set-parameters "skim"
                             :face 'default
                             :follow #'+reference/org-mac-skim-open
                             :export (lambda (path desc backend)
                                       (cond ((eq 'html backend)
                                              (format "<a href=\"skim:%s\" >%s</a>"
                                                      (org-html-encode-plain-text path)
                                                      desc)))))
    (defun +org-private/as-get-skim-page-link ()
      (do-applescript
       (concat
        "tell application \"Skim\"\n"
        "set theDoc to front document\n"
        "set theTitle to (name of theDoc)\n"
        "set thePath to (path of theDoc)\n"
        "set thePage to (get index for current page of theDoc)\n"
        "set theSelection to selection of theDoc\n"
        "set theContent to contents of (get text for theSelection)\n"
        "if theContent is missing value then\n"
        "    set theContent to theTitle & \", p. \" & thePage\n"
        (when org-mac-Skim-highlight-selection-p
          (concat
           "else\n"
           "    tell theDoc\n"
           "        set theNote to make note with data theSelection with properties {type:highlight note}\n"
           "         set text of theNote to (get text for theSelection)\n"
           "    end tell\n"))
        "end if\n"
        "set theLink to \"skim://\" & thePath & \"::\" & thePage & "
        "\"::split::\" & theContent\n"
        "end tell\n"
        "return theLink as string\n")))

    (advice-add 'as-get-skim-page-link :override #'+org-private/as-get-skim-page-link))

  (defun +org-private/*org-ctrl-c-ctrl-c-counsel-org-tag ()
    "Hook for `org-ctrl-c-ctrl-c-hook' to use `counsel-org-tag'."
    (if (save-excursion (beginning-of-line) (looking-at "[ \t]*$"))
        (or (run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-final-hook)
            (user-error "C-c C-c can do nothing useful at this location"))
      (let* ((context (org-element-context))
             (type (org-element-type context)))
        (case type
          ;; When at a link, act according to the parent instead.
          (link (setq context (org-element-property :parent context))
                (setq type (org-element-type context)))
          ;; Unsupported object types: refer to the first supported
          ;; element or object containing it.
          ((bold code entity export-snippet inline-babel-call inline-src-block
                 italic latex-fragment line-break macro strike-through subscript
                 superscript underline verbatim)
           (setq context
                 (org-element-lineage
                  context '(radio-target paragraph verse-block table-cell)))))
        ;; For convenience: at the first line of a paragraph on the
        ;; same line as an item, apply function on that item instead.
        (when (eq type 'paragraph)
          (let ((parent (org-element-property :parent context)))
            (when (and (eq (org-element-type parent) 'item)
                       (= (line-beginning-position)
                          (org-element-property :begin parent)))
              (setq context parent type 'item))))
        (case type
          ((headline inlinetask)
           (save-excursion (goto-char (org-element-property :begin context))
                           (call-interactively 'counsel-org-tag)) t)))))
  (add-hook 'org-ctrl-c-ctrl-c-hook '+org-private/*org-ctrl-c-ctrl-c-counsel-org-tag)
  (advice-add 'org-shiftcontrolup :override #'*org/shiftcontrolup)
  (advice-add 'org-shiftcontroldown :override #'*org/shiftcontroldown)

  (defun *org-eldoc-get-timestamp (str)
    "Return timestamp if on a headline or nil."
    (if str
        (concat
         (let ((deadline (org-entry-get (point) "DEADLINE" t))
               (scheduled (org-entry-get (point) "SCHEDULED" t))
               (closed (org-entry-get (point) "CLOSED" t))
               timestamp)
           (if deadline (setq timestamp (concat timestamp (propertize (substring deadline 1 -1) 'face 'org-deadline-custom) " ")))
           (if scheduled (setq timestamp (concat timestamp (propertize (substring scheduled 1 -1) 'face 'org-scheduled-custom) " ")))
           (if closed (setq timestamp (concat timestamp (propertize (substring closed 1 -1) 'face 'org-closed-custom) " ")))
           timestamp)
         str)
      nil))

  (advice-add 'org-eldoc-get-breadcrumb :filter-return #'*org-eldoc-get-timestamp)

  (defun *org-format-outline-path-normalize (str)
    (add-face-text-property
     0 (length str)
     '(:height 1) nil str)
    str)
  (advice-add 'org-format-outline-path :filter-return #'*org-format-outline-path-normalize))

;;
;; `org-mode' hooks
;;

(def-package! org-clock
  :commands org-clock-save
  :hook (org-mode . org-clock-load)
  ;; (setq org-clock-persist t
  ;;       org-clock-persist-file (expand-file-name ".org-clock-persist-data.el" org-directory))
  ;; (add-hook 'kill-emacs-hook 'org-clock-save)
  )
