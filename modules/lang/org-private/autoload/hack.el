;;; lang/org-private/autoload/hack.el -*- lexical-binding: t; -*-

;;;###autoload
(defun *org-eldoc-get-timestamp (str)
  "Return timestamp if on a headline or nil."
  (if str
      (concat
       (let ((deadline (org-entry-get
                        (point)
                        "DEADLINE"
                        t))
             (scheduled (org-entry-get
                         (point)
                         "SCHEDULED"
                         t))
             (closed (org-entry-get
                      (point)
                      "CLOSED"
                      t))
             timestamp)
         (if deadline
             (setq timestamp
                   (concat
                    timestamp
                    (propertize
                     (substring deadline 1 -1)
                     'face
                     'org-deadline-custom)
                    " ")))
         (if scheduled
             (setq timestamp
                   (concat
                    timestamp
                    (propertize
                     (substring scheduled 1 -1)
                     'face
                     'org-scheduled-custom)
                    " ")))
         (if closed
             (setq timestamp
                   (concat
                    timestamp
                    (propertize
                     (substring closed 1 -1)
                     'face
                     'org-closed-custom)
                    " ")))
         timestamp)
       str)
    nil))

;;;###autoload
(defun *org-format-outline-path-normalize (str)
  (add-face-text-property
   0
   (length str)
   '(:height 1)
   nil
   str)
  str)

;;;###autoload
(defun +org-private/*org-ctrl-c-ctrl-c-counsel-org-tag ()
  "Hook for `org-ctrl-c-ctrl-c-hook' to use `counsel-org-tag'."
  (if (save-excursion
        (beginning-of-line)
        (looking-at "[ \t]*$"))
      (or (run-hook-with-args-until-success
           'org-ctrl-c-ctrl-c-final-hook)
          (user-error
           "C-c C-c can do nothing useful at this location"))
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      (case type
        ;; When at a link, act according to the parent instead.
        (link
         (setq context
               (org-element-property
                :parent context))
         (setq type
               (org-element-type context)))
        ;; Unsupported object types: refer to the first supported
        ;; element or object containing it.
        ((bold
          code
          entity
          export-snippet
          inline-babel-call
          inline-src-block
          italic
          latex-fragment
          line-break
          macro
          strike-through
          subscript
          superscript
          underline
          verbatim)
         (setq context
               (org-element-lineage
                context
                '(radio-target
                  paragraph
                  verse-block
                  table-cell)))))
      ;; For convenience: at the first line of a paragraph on the
      ;; same line as an item, apply function on that item instead.
      (when (eq type 'paragraph)
        (let ((parent (org-element-property
                       :parent context)))
          (when (and (eq (org-element-type parent)
                         'item)
                     (= (line-beginning-position)
                        (org-element-property
                         :begin parent)))
            (setq context
                  parent
                  type
                  'item))))
      (case type
        ((headline inlinetask)
         (save-excursion
           (goto-char
            (org-element-property
             :begin context))
           (call-interactively
            'counsel-org-tag))
         t)))))

;;;###autoload
(defun +org/insert-item-with-ts ()
  "When on org timestamp item insert org timestamp item with current time.
This holds only for inactive timestamps."
  (interactive)
  (when (save-excursion
          (let ((item-pos (org-in-item-p)))
            (when item-pos
              (goto-char item-pos)
              (org-list-at-regexp-after-bullet-p
               org-ts-regexp-inactive))))
    (let ((item-pos (org-in-item-p))
          (pos (point)))
      (assert item-pos)
      (goto-char item-pos)
      (let* ((struct (org-list-struct))
             (prevs (org-list-prevs-alist struct))
             (s (concat
                 (with-temp-buffer
                   (org-insert-time-stamp nil t t)
                   (buffer-string))
                 " ")))
        (setq struct
              (org-list-insert-item
               pos
               struct
               prevs
               nil
               s))
        (org-list-write-struct
         struct
         (org-list-parents-alist struct))
        (looking-at
         org-list-full-item-re)
        (goto-char (match-end 0))
        (end-of-line)))
    t))

;;;###autoload
(defun +org/insert-go-eol ()
  (when (bound-and-true-p evil-mode)
    (evil-insert 1))
  (end-of-line))

;;;###autoload
(defun +org-private/org-html--tags (tags info)
  "Format TAGS into HTML.
INFO is a plist containing export options."
  (when tags
    (format
     "
<span class=\"tag\">%s</span>
"
     (mapconcat
      (lambda (tag)
        (format
         "<span class=\"%s\">%s</span>"
         (concat
          (plist-get
           info
           :html-tag-class-prefix)
          (org-html-fix-class-name tag))
         tag))
      tags
      " "))))

;;;###autoload
(defun +org-private/org-add-ids-to-headlines-in-file ()
  "Add CUSTOM_ID properties to all headlines in the current file"
  (interactive)
  (if (string-equal
       default-directory
       org-directory)
      (save-excursion
        (widen)
        (goto-char (point-min))
        (org-map-entries
         'org-id-get-create))))

;;;###autoload
(defun org-font-lock-add-priority-faces (limit)
  "Add the special priority faces."
  (while (re-search-forward
          "^\\*+ .*?\\(\\[\\(#\\(.\\)\\)\\]\\)"
          limit
          t)
    (add-face-text-property
     (match-beginning 1)
     (match-end 1)
     'org-priority-hide)
    (add-face-text-property
     (match-beginning 2)
     (match-end 2)
     (org-get-priority-face
      (string-to-char
       (match-string 3))))))

;;;###autoload
(defun *org-set-font-lock-defaults ()
  "Set font lock defaults for the current buffer."
  (let* ((em org-fontify-emphasized-text)
         (lk org-highlight-links)
         (org-font-lock-extra-keywords (list
                                        ;; Call the hook
                                        '(org-font-lock-hook)
                                        ;; Headlines
                                        `(,(if org-fontify-whole-heading-line
                                               "^\\(\\**\\)\\(\\* \\)\\(.*
?\\)"
                                             "^\\(\\**\\)\\(\\* \\)\\(.*\\)")
                                          (1 (org-get-level-face 1))
                                          (2 (org-get-level-face 2))
                                          (3 (org-get-level-face 3)))
                                        ;; Table lines
                                        '("^[ \t]*\\(\\(|\\|\\+-[-+]\\).*\\S-\\)"
                                          (1 'org-table t))
                                        ;; Table internals
                                        '("^[ \t]*|\\(?:.*?|\\)? *\\(:?=[^|
]*\\)"
                                          (1 'org-formula t))
                                        '("^[ \t]*| *\\([#*]\\) *|"
                                          (1 'org-formula t))
                                        '("^[ \t]*|\\( *\\([$!_^/]\\) *|.*\\)|"
                                          (1 'org-formula t))
                                        '("| *\\(<[lrc]?[0-9]*>\\)"
                                          (1 'org-formula t))
                                        ;; Drawers
                                        '(org-fontify-drawers)
                                        ;; Properties
                                        (list
                                         org-property-re
                                         '(1 'org-special-keyword t)
                                         '(3 'org-property-value t))
                                        ;; Special keywords
                                        (list
                                         (concat
                                          "\\<\\(DEADLINE: \\)"
                                          org-ts-regexp-both-braket)
                                         '(1 'org-deadline-custom t)
                                         '(2
                                           'org-deadline-custom-braket
                                           t)
                                         '(3 'org-deadline-custom t)
                                         '(4
                                           'org-deadline-custom-braket
                                           t))
                                        (list
                                         (concat
                                          "\\<\\(SCHEDULED: \\)"
                                          org-ts-regexp-both-braket)
                                         '(1 'org-scheduled-custom t)
                                         '(2
                                           'org-scheduled-custom-braket
                                           t)
                                         '(3 'org-scheduled-custom t)
                                         '(4
                                           'org-scheduled-custom-braket
                                           t))
                                        (list
                                         (concat
                                          "\\<\\(CLOSED: \\)"
                                          org-ts-regexp-both-braket)
                                         '(1 'org-closed-custom t)
                                         '(2 'org-closed-custom-braket t)
                                         '(3 'org-closed-custom t)
                                         '(4 'org-closed-custom-braket t))
                                        (list
                                         (concat "\\<" org-clock-string)
                                         '(0
                                           'org-special-keyword
                                           prepend))
                                        ;; Link related fontification.
                                        '(org-activate-links)
                                        (when (memq 'tag lk)
                                          '(org-activate-tags
                                            (1 'org-tag prepend)))
                                        (when (memq 'radio lk)
                                          '(org-activate-target-links
                                            (1 'org-link t)))
                                        (when (memq 'date lk)
                                          '(org-activate-dates
                                            (0 'org-date t)))
                                        ;; (when (memq 'date lk) '(org-activate-dates (0 'org-date append)))
                                        (when (memq 'footnote lk)
                                          '(org-activate-footnote-links))
                                        ;; Targets.
                                        (list
                                         org-radio-target-regexp
                                         '(0 'org-target t))
                                        (list
                                         org-target-regexp
                                         '(0 'org-target t))
                                        ;; Diary sexps.
                                        '("^&?%%(.*\\|<%%([^>\n]*?>"
                                          (0 'org-sexp-date t))
                                        ;; Macro
                                        '(org-fontify-macros)
                                        ;; TODO keyword
                                        (list
                                         (format
                                          org-heading-keyword-regexp-format
                                          org-todo-regexp)
                                         '(2 (org-get-todo-face 2) t))
                                        ;; DONE
                                        (if org-fontify-done-headline
                                            (list
                                             (format
                                              org-heading-keyword-regexp-format
                                              (concat
                                               "\\(?:"
                                               (mapconcat
                                                'regexp-quote
                                                org-done-keywords
                                                "\\|")
                                               "\\)"))
                                             '(2 'org-headline-done t))
                                          nil)
                                        ;; Priorities
                                        '(org-font-lock-add-priority-faces)
                                        ;; Tags
                                        '(org-font-lock-add-tag-faces)
                                        ;; Tags groups
                                        (when (and org-group-tags
                                                   org-tag-groups-alist)
                                          (list
                                           (concat
                                            org-outline-regexp-bol
                                            ".+\\(:"
                                            (regexp-opt
                                             (mapcar
                                              'car
                                              org-tag-groups-alist))
                                            ":\\).*$")
                                           '(1 'org-tag-group prepend)))
                                        ;;
                                        ;; Special keywords
                                        ;; (list (concat "\\<" org-deadline-string) '(0 'org-special-keyword t))
                                        ;; (list (concat "\\<" org-scheduled-string) '(0 'org-special-keyword t))
                                        ;; (list (concat "\\<" org-closed-string) '(0 'org-special-keyword t))
                                        ;; (list (concat "\\<" org-clock-string) '(0 'org-special-keyword t))
                                        ;; Emphasis
                                        (when em
                                          '(org-do-emphasis-faces))
                                        ;; Checkboxes
                                        '("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\(\\[[- X]\\]\\)"
                                          1
                                          'org-checkbox
                                          prepend)
                                        (when (cdr (assq 'checkbox
                                                         org-list-automatic-rules))
                                          '("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                                            (0
                                             (org-get-checkbox-statistics-face)
                                             t)))
                                        ;; Description list items
                                        '("^[ \t]*[-+*][ \t]+\\(.*?[ \t]+::\\)\\([ \t]+\\|$\\)"
                                          1
                                          'org-list-dt
                                          prepend)
                                        ;; ARCHIVEd headings
                                        (list
                                         (concat
                                          org-outline-regexp-bol
                                          "\\(.*:"
                                          org-archive-tag
                                          ":.*\\)")
                                         '(1 'org-archived prepend))
                                        ;; Specials
                                        '(org-do-latex-and-related)
                                        '(org-fontify-entities)
                                        '(org-raise-scripts)
                                        ;; Code
                                        '(org-activate-code
                                          (1 'org-code t))
                                        ;; COMMENT
                                        (list
                                         (format
                                          "^\\*+\\(?: +%s\\)?\\(?: +\\[#[A-Z0-9]\\]\\)? +\\(?9:%s\\)\\(?: \\|$\\)"
                                          org-todo-regexp
                                          org-comment-string)
                                         '(9 'org-special-keyword t))
                                        ;; Blocks and meta lines
                                        '(org-fontify-meta-lines-and-blocks))))
    (setq org-font-lock-extra-keywords
          (delq nil
                org-font-lock-extra-keywords))
    (run-hooks
     'org-font-lock-set-keywords-hook)
    ;; Now set the full font-lock-keywords
    (setq-local
     org-font-lock-keywords
     org-font-lock-extra-keywords)
    (setq-local
     font-lock-defaults
     '(org-font-lock-keywords
       t
       nil
       nil
       backward-paragraph))
    (setq-local
     font-lock-extend-after-change-region-function
     #'org-fontify-extend-region)
    (kill-local-variable
     'font-lock-keywords)
    nil))
