;;; outshine.el --- my cleaned outline.el

;;; Commentary:

;;; Code:
;;* Requires
(eval-when-compile
  (require 'cl))
(require 'outline)
;; necessary before Emacs 24.3
;; (require 'newcomment)
;; (require 'cl-lib)

;;; Declarations

(declare-function imenu-choose-buffer-index "imenu")

;;; Variables
;;;; Consts

(defconst outshine-version "2.0"
  "outshine version number.")

(defconst outshine-max-level 8
  "Maximal level of headlines recognized.")

;; copied from org-source.el
(defconst outshine-level-faces '(outshine-level-1
  outshine-level-2 outshine-level-3 outshine-level-4
  outshine-level-5 outshine-level-6 outshine-level-7
  outshine-level-8))

(defconst outshine-outline-heading-end-regexp "\n"
  "Global default value of `outline-heading-end-regexp'.
Used to override any major-mode specific file-local settings")

;; was "[;]+"
(defconst outshine-oldschool-elisp-outline-regexp-base
  (format "[;]\\{1,%d\\}" outshine-max-level)
  "Oldschool Emacs Lisp base for calculating the outline-regexp")

(defconst outshine-comment-tag "comment"
  "The tag that marks a subtree as comment.
A comment subtree does not open during visibility cycling.")

;; (defconst outshine-latex-documentclass-regexp
;;   "^[[:space:]]*\\\\documentclass\\(?:\\[.+]\\)?{\\(.+\\)}"
;;   "Regexp matching the document class in a latex doc (in submatch
;;   1)")

;;;; Vars

;; "\C-c" conflicts with other modes like e.g. ESS
(defvar outline-minor-mode-prefix "\M-#"
  "New outline-minor-mode prefix.
Does not really take effect when set in the `outshine' library.
Instead, it must be set in your init file *before* the `outline'
library is loaded, see the installation tips in the comment
section of `outshine'.")

;; from `outline-magic'
(defvar outline-promotion-headings nil
  "A sorted list of headings used for promotion/demotion commands.
Set this to a list of headings as they are matched by `outline-regexp',
top-level heading first.  If a mode or document needs several sets of
outline headings (for example numbered and unnumbered sections), list
them set by set, separated by a nil element.  See the example for
`texinfo-mode' in the file commentary.")
(make-variable-buffer-local 'outline-promotion-headings)

(defvar outshine-delete-leading-whitespace-from-outline-regexp-base-p nil
  "If non-nil, delete leading whitespace from outline-regexp-base.")
(make-variable-buffer-local
 'outshine-delete-leading-whitespace-from-outline-regexp-base-p)

(defvar outshine-enforce-no-comment-padding-p nil
  "If non-nil, make sure no comment-padding is used in heading.")
(make-variable-buffer-local
 'outshine-enforce-no-comment-padding-p)

(defvar outshine-outline-regexp-base ""
  "Actual base for calculating the outline-regexp")

(defvar outshine-normalized-comment-start ""
  "Comment-start regexp without leading and trailing whitespace")
(make-variable-buffer-local
 'outshine-normalized-comment-start)

(defvar outshine-normalized-comment-end ""
  "Comment-end regexp without leading and trailing whitespace")
(make-variable-buffer-local
 'outshine-normalized-comment-end)

(defvar outshine-normalized-outline-regexp-base ""
  "Outline-regex-base without leading and trailing whitespace")
(make-variable-buffer-local
 'outshine-normalized-outline-regexp-base)

;; show number of hidden lines in folded subtree
(defvar outshine-show-hidden-lines-cookies-p nil
  "If non-nil, commands for hidden-lines cookies are activated.")

;; remember if hidden-lines cookies are shown or hidden
(defvar outshine-hidden-lines-cookies-on-p nil
  "If non-nil, hidden-lines cookies are shown, otherwise hidden.")

(defvar outshine-imenu-default-generic-expression nil
  "Expression assigned by default to `imenu-generic-expression'.")
(make-variable-buffer-local
 'outshine-imenu-default-generic-expression)

(defvar outshine-imenu-generic-expression nil
  "Expression assigned to `imenu-generic-expression'.")
(make-variable-buffer-local
 'outshine-imenu-generic-expression)

(defvar outshine-open-comment-trees nil
  "Cycle comment-subtrees anyway when non-nil.")

(defvar outshine-current-buffer-visibility-state nil
  "Stores current visibility state of buffer.")

(defvar outshine-imenu-preliminary-generic-expression nil
  "Imenu variable.")


;;;; Hooks

(defvar outshine-hook nil
  "Functions to run after `outshine' is loaded.")

;;;; Faces

(defgroup outshine nil
  "Enhanced library for outline navigation in source code buffers."
  :prefix "outshine-"
  :group 'lisp)


;;;;; Custom Vars

(defcustom outshine-imenu-show-headlines-p t
  "Non-nil means use calculated outline-regexp for imenu."
  :group 'outshine
  :type 'boolean)

(defcustom outshine-fontify t
  ;; `quote' instead of ' to avoid conversion of ' in for example C-h v
  ;; (`describe-variable').
  "When to fontify the outshine headings in a buffer.

Possible values are:

 `t'        Always (the default).
 `nil'      Never.
 function   A Lisp predicate function with no arguments. For example
            `(lambda () (not (derived-mode-p (quote prog-mode))))'
            fontifies only when not in a programming mode.

`t' and `nil' can be used for a file local variable to make an
exception for certain files or to be independent of the user's
customization."
  :group 'outshine
  :type '(choice :value ""
                 (const :tag "Always (the default)" t)
                 (const :tag "Never" nil)
                 (function :tag "Function"))
  :safe (lambda (v) (memq v '(t nil))))

;; from `org'
(defcustom outshine-fontify-whole-heading-line nil
  "Non-nil means fontify the whole line for headings.
This is useful when setting a background color for the
poutshine-level-* faces."
  :group 'outshine
  :type 'boolean)

(defcustom outshine-outline-regexp-outcommented-p t
  "Non-nil if regexp-base is outcommented to calculate outline-regexp."
  :group 'outshine
  :type 'boolean)

;; was "[][+]"
(defcustom outshine-outline-regexp-special-chars
  "[][}{,+[:digit:]\\]"
  "Regexp for detecting (special) characters in outline-regexp.
These special chars will be stripped when the outline-regexp is
transformed into a string, e.g. when the outline-string for a
certain level is calculated. "
  :group 'outshine
  :type 'regexp)

;; from `outline-magic'
(defcustom outline-cycle-emulate-tab nil
  "Where should `outline-cycle' emulate TAB.
nil    Never
white  Only in completely white lines
t      Everywhere except in headlines"
  :group 'outlines
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Only in completely white lines" white)
                 (const :tag "Everywhere except in headlines" t)
                 ))

;; from `outline-magic'
(defcustom outline-structedit-modifiers '(meta)
  "List of modifiers for outline structure editing with the arrow keys."
  :group 'outlines
  :type '(repeat symbol))

;; startup options
(defcustom outshine-startup-folded-p nil
  "Non-nil means files will be opened with all but top level headers folded."
  :group 'outshine
  :type 'boolean)

(defcustom outshine-hidden-lines-cookie-left-delimiter "["
  "Left delimiter of cookie that shows number of hidden lines."
  :group 'outshine
  :type 'string)

(defcustom outshine-hidden-lines-cookie-right-delimiter "]"
  "Left delimiter of cookie that shows number of hidden lines."
  :group 'outshine
  :type 'string)

(defcustom outshine-hidden-lines-cookie-left-signal-char "#"
  "Left signal character of cookie that shows number of hidden lines."
  :group 'outshine
  :type 'string)

(defcustom outshine-hidden-lines-cookie-right-signal-char ""
  "Right signal character of cookie that shows number of hidden lines."
  :group 'outshine
  :type 'string)

(defcustom outshine-regexp-base-char "*"
  "Character used in outline-regexp base."
  :group 'outshine
  :type 'string)



;; old regexp: "[*]+"
(defvar outshine-default-outline-regexp-base
  (format "[%s]\\{1,%d\\}"
          outshine-regexp-base-char outshine-max-level)
  "Default base for calculating the outline-regexp")

;; TODO delete this line  "\\(\\[\\)\\([[:digit:]+]\\)\\( L\\]\\)"
(defvar outshine-hidden-lines-cookie-format-regexp
  (concat
   "\\( "
   (regexp-quote outshine-hidden-lines-cookie-left-delimiter)
   (regexp-quote outshine-hidden-lines-cookie-left-signal-char)
   "\\)"
   "\\([[:digit:]]+\\)"
   "\\("
   (regexp-quote outshine-hidden-lines-cookie-right-signal-char)
   ;; FIXME robust enough?
   (format "\\%s" outshine-hidden-lines-cookie-right-delimiter)
   "\\)")
  "Matches cookies that show number of hidden lines for folded subtrees.")

(defvar outshine-cycle-silently nil
  "Suppress visibility-state-change messages when non-nil.")

(defcustom outshine-org-style-global-cycling-at-bob-p nil
  "Cycle globally if cursor is at beginning of buffer and not at a headline.

This makes it possible to do global cycling without having to use
S-TAB or C-u TAB.  For this special case to work, the first line
of the buffer must not be a headline -- it may be empty or some
other text. When this option is nil, don't do anything special at
the beginning of the buffer."
  :group 'outshine
  :type 'boolean)

(defcustom outshine-self-insert-cluster-for-undo
  (or (featurep 'xemacs) (version<= emacs-version "24.1"))
  "Non-nil means cluster self-insert commands for undo when possible.
If this is set, then, like in the Emacs command loop, 20 consecutive
characters will be undone together.
This is configurable, because there is some impact on typing performance."
  :group 'outshine
  :type 'boolean)

;; (defcustom outshine-latex-classes
;;   '(("scrbook" . ((1 . "^[[:space:]]*\\\\part\\*?{\\(.+\\)}")
;; 		  (2 . "^[[:space:]]*\\\\chapter\\*?{\\(.+\\)}")
;; 		  (3 . "^[[:space:]]*\\\\section\\*?{\\(.+\\)}")
;; 		  (4 . "^[[:space:]]*\\\\subsection\\*?{\\(.+\\)}")
;; 		  (5 . "^[[:space:]]*\\\\subsubsection\\*?{\\(.+\\)}")
;; 		  (6 . "^[[:space:]]*\\\\paragraph\\*?{\\(.+\\)}")
;; 		  (7 . "^[[:space:]]*\\\\subparagraph\\*?{\\(.+\\)}")))
;;     ("book" . ((1 . "^[[:space:]]*\\\\part\\*?{\\(.+\\)}")
;; 	       (2 . "^[[:space:]]*\\\\chapter\\*?{\\(.+\\)}")
;; 	       (3 . "^[[:space:]]*\\\\section\\*?{\\(.+\\)}")
;; 	       (4 . "^[[:space:]]*\\\\subsection\\*?{\\(.+\\)}")
;; 	       (5 . "^[[:space:]]*\\\\subsubsection\\*?{\\(.+\\)}")))
;;     ("report" . ((1 . "^[[:space:]]*\\\\part\\*?{\\(.+\\)}")
;; 		 (2 . "^[[:space:]]*\\\\chapter\\*?{\\(.+\\)}")
;; 		 (3 . "^[[:space:]]*\\\\section\\*?{\\(.+\\)}")
;; 		 (4 . "^[[:space:]]*\\\\subsection\\*?{\\(.+\\)}")
;; 		 (5 . "^[[:space:]]*\\\\subsubsection\\*?{\\(.+\\)}")))
;;     ("scrartcl" . ((1 . "^[[:space:]]*\\\\section\\*?{\\(.+\\)}")
;; 		   (2 . "^[[:space:]]*\\\\subsection\\*?{\\(.+\\)}")
;; 		   (3 . "^[[:space:]]*\\\\subsubsection\\*?{\\(.+\\)}")
;; 		   (4 . "^[[:space:]]*\\\\paragraph\\*?{\\(.+\\)}")
;; 		   (5 . "^[[:space:]]*\\\\subparagraph\\*?{\\(.+\\)}")))
;;     ("article" . ((1 . "^[[:space:]]*\\\\section\\*?{\\(.+\\)}")
;; 		  (2 . "^[[:space:]]*\\\\subsection\\*?{\\(.+\\)}")
;; 		  (3 . "^[[:space:]]*\\\\subsubsection\\*?{\\(.+\\)}")
;; 		  (4 . "^[[:space:]]*\\\\paragraph\\*?{\\(.+\\)}")
;; 		  (5 . "^[[:space:]]*\\\\subparagraph\\*?{\\(.+\\)}"))))
;;   "Sectioning structure of LaTeX classes.
;; For each class, the outline level and a regexp matching the latex
;; section are given (with section title in submatch 1)."
;;   :group 'outshine
;;   :type '(alist :key-type string
;;                 :value-type alist))

(defcustom outshine-preserve-delimiter-whitespace nil
  "Non-nil means that whitespace present at the start or end of
`comment-start' is preserved when matching headline syntax. By
default, such space is removed to support language modes which
erroneously include it in `comment-start', but for other
languages, such as Haskell, the trailing whitespace is
significant."
  :group 'outshine
  :type 'boolean)

;;; Defuns
;;;; Advices



;; from http://emacswiki.org/emacs/ElispCookbook#toc6
(defun outshine-chomp (str)
  "Chomp leading and trailing whitespace from STR."
  (save-excursion
    (save-match-data
      (while (string-match
              "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
              str)
        (setq str (replace-match "" t t str)))
      str)))

(defun outshine-set-outline-regexp-base ()
  "Return the actual outline-regexp-base."
  (if (and
       (not (outshine-modern-header-style-in-elisp-p))
       (eq major-mode 'emacs-lisp-mode))
      (progn
        (setq outshine-enforce-no-comment-padding-p t)
        (setq outshine-outline-regexp-base
              outshine-oldschool-elisp-outline-regexp-base))
    (setq outshine-enforce-no-comment-padding-p nil)
    (setq outshine-outline-regexp-base
          outshine-default-outline-regexp-base)))

(defun outshine-normalize-regexps ()
  "Chomp leading and trailing whitespace from outline regexps."
  (and comment-start
       (setq outshine-normalized-comment-start
             (if outshine-preserve-delimiter-whitespace
                 comment-start
                 (outshine-chomp comment-start))))
  (and comment-end
       (setq outshine-normalized-comment-end
             (outshine-chomp comment-end)))
  (and outshine-outline-regexp-base
       (setq outshine-normalized-outline-regexp-base
             (outshine-chomp outshine-outline-regexp-base))))

;;;;; Calculate outline-regexp and outline-level

;; dealing with special case of oldschool headers in elisp (;;;+)
(defun outshine-modern-header-style-in-elisp-p (&optional buffer)
  "Return nil, if there is no match for a outshine-style header.
Searches in BUFFER if given, otherwise in current buffer."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (re-search-forward
         ;; (format "^;; [%s]+ " outshine-regexp-base-char)
         (format "^;; [%s]\\{1,%d\\} "
                 outshine-regexp-base-char outshine-max-level)
         nil 'NOERROR)))))

(defun outshine-calc-comment-region-starter ()
  "Return comment-region starter as string.
Based on `comment-start' and `comment-add'."
  (if (or (not comment-add) (eq comment-add 0))
      outshine-normalized-comment-start
    (let ((comment-add-string outshine-normalized-comment-start))
      (dotimes (i comment-add comment-add-string)
        (setq comment-add-string
              (concat comment-add-string outshine-normalized-comment-start))))))

(defun outshine-calc-comment-padding ()
  "Return comment-padding as string"
  (cond
   ;; comment-padding is nil
   ((not comment-padding) " ")
   ;; comment-padding is integer
   ((integer-or-marker-p comment-padding)
    (let ((comment-padding-string ""))
      (dotimes (i comment-padding comment-padding-string)
        (setq comment-padding-string
              (concat comment-padding-string " ")))))
   ;; comment-padding is string
   ((stringp comment-padding)
    comment-padding)
   (t (error "No valid comment-padding"))))

(defun outshine-calc-outline-regexp ()
  "Calculate the outline regexp for the current mode."
  (concat
   (and outshine-outline-regexp-outcommented-p
         ;; regexp-base outcommented, but no 'comment-start' defined
         (or comment-start
             (message (concat
                       "Cannot calculate outcommented outline-regexp\n"
                       "without 'comment-start' character defined!")))
         (concat
          ;; comment-start
          ;; (outshine-calc-comment-region-starter)
          (regexp-quote
           (outshine-calc-comment-region-starter))
          ;; comment-padding
          (if outshine-enforce-no-comment-padding-p
              ""
            (outshine-calc-comment-padding))))
   ;; regexp-base
   outshine-normalized-outline-regexp-base
   " "))

;; TODO how is this called (match-data?) 'looking-at' necessary?
(defun outshine-calc-outline-level ()
  "Calculate the right outline level for the
  outshine-outline-regexp"
  (save-excursion
    (save-match-data
      (and
       (looking-at (outshine-calc-outline-regexp))
       ;; ;; FIXME this works?
       ;; (looking-at outline-regexp)
       (let ((m-strg (match-string-no-properties 0)))
         (if outshine-enforce-no-comment-padding-p
             ;; deal with oldschool elisp headings (;;;+)
             (setq m-strg
                   (split-string
                    (substring m-strg 2)
                    nil
                    'OMIT-NULLS))
           ;; orgmode style elisp heading (;; *+)
           (setq m-strg
                 (split-string
                  m-strg
                  (format "%s" outshine-normalized-comment-start)
                  'OMIT-NULLS)))
         (length
          (mapconcat
           (lambda (str)
             (car
              (split-string
               str
               " "
               'OMIT-NULLS)))
           m-strg
           "")))
       ))))

;;;;; Set outline-regexp und outline-level

(defun outshine-set-local-outline-regexp-and-level
  (start-regexp &optional fun end-regexp)
   "Set `outline-regexp' locally to START-REGEXP.
Set optionally `outline-level' to FUN and
`outline-heading-end-regexp' to END-REGEXP."
        (make-local-variable 'outline-regexp)
        (setq outline-regexp start-regexp)
        (and fun
             (make-local-variable 'outline-level)
             (setq outline-level fun))
        (and end-regexp
             (make-local-variable 'outline-heading-end-regexp)
             (setq outline-heading-end-regexp end-regexp)))

;;;;; Show number of lines in hidden body

;; Calc and show line number of hidden body for all visible headlines
(defun outshine-write-hidden-lines-cookies ()
  "Show line number of hidden lines in folded headline."
  (and outshine-show-hidden-lines-cookies-p
       (save-excursion
         (goto-char (point-min))
         (and (outline-on-heading-p)
              (outshine-hidden-lines-cookie-status-changed-p)
              (outshine-set-hidden-lines-cookie))
         (while (not (eobp))
           (outline-next-visible-heading 1)
           (and (outline-on-heading-p)
                (outshine-hidden-lines-cookie-status-changed-p)
                (outshine-set-hidden-lines-cookie))))))

(defun outshine-hidden-lines-cookie-status-changed-p ()
  "Return non-nil if hidden-lines cookie needs modification."
  (save-excursion
    (save-match-data
      (or (not (outline-body-visible-p))
          (re-search-forward
           outshine-hidden-lines-cookie-format-regexp
           (line-end-position)
           'NO-ERROR)))))

(defun outshine-set-hidden-lines-cookie ()
  "Calculate and set number of hidden lines in folded headline."
  (let* ((folded-p (not (outline-body-visible-p)))
         (line-num-current-header (line-number-at-pos))
         (line-num-next-visible-header
          (save-excursion
            (outline-next-visible-heading 1)
            (line-number-at-pos)))
         (body-lines
          (1- (- line-num-next-visible-header line-num-current-header))))
    (if (re-search-forward
         outshine-hidden-lines-cookie-format-regexp
         (line-end-position)
         'NO-ERROR)
        (cond
         ((not folded-p) (replace-match ""))
         (folded-p (replace-match (format "%s" body-lines) nil nil nil 2)))
      (show-entry)
      (save-excursion
        (end-of-line)
        (insert
         (format
          " %s%s%s%s%s"
          outshine-hidden-lines-cookie-left-delimiter
          outshine-hidden-lines-cookie-left-signal-char
          body-lines
          outshine-hidden-lines-cookie-right-signal-char
          outshine-hidden-lines-cookie-right-delimiter)))
      (hide-entry))))

;; ;; FIXME
;; ;; outline-flag-region: Variable binding depth exceeds max-specpdl-size
;; (add-hook 'outline-view-change-hook
;;           'outshine-write-hidden-lines-cookies)

;;;;; Return outline-string at given level

(defun outshine-calc-outline-string-at-level (level)
  "Return outline-string at level LEVEL."
  (let ((base-string (outshine-calc-outline-base-string-at-level level)))
    (if (not outshine-outline-regexp-outcommented-p)
        base-string
      (concat (outshine-calc-comment-region-starter)
              (if outshine-enforce-no-comment-padding-p
                  ""
                (outshine-calc-comment-padding))
              base-string
              " "))))

(defun outshine-calc-outline-base-string-at-level (level)
  "Return outline-base-string at level LEVEL."
  (let* ((star (outshine-transform-normalized-outline-regexp-base-to-string))
         (stars star))
       (dotimes (i (1- level) stars)
         (setq stars (concat stars star)))))

(defun outshine-transform-normalized-outline-regexp-base-to-string ()
  "Transform 'outline-regexp-base' to string by stripping off special chars."
  (replace-regexp-in-string
   outshine-outline-regexp-special-chars
   ""
   outshine-normalized-outline-regexp-base))

;; make demote/promote from `outline-magic' work
(defun outshine-make-promotion-headings-list (max-level)
  "Make a sorted list of headings used for promotion/demotion commands.
Set this to a list of MAX-LEVEL headings as they are matched by `outline-regexp',
top-level heading first."
  (let ((list-of-heading-levels
         `((,(outshine-calc-outline-string-at-level 1) . 1))))
    (dotimes (i (1- max-level) list-of-heading-levels)
            (add-to-list
             'list-of-heading-levels
             `(,(outshine-calc-outline-string-at-level (+ i 2)) . ,(+ i 2))
             'APPEND))))

;;;;; Fontify the headlines

(defsubst outshine-font-lock-flush ()
  "Calls `font-lock-flush' or equivalent.
Compatibility with Emacs versions <25."
  (if (fboundp #'font-lock-flush)
      (font-lock-flush)
    ;; Copied from Emacs 25 font-lock.el, changed to call
    ;; `jit-lock-refontify' directly
    (and font-lock-mode
         font-lock-fontified
         (jit-lock-refontify))))

(defun outshine-fontify-headlines (outline-regexp)
  "Calculate heading regexps for font-lock mode."
  (let* ((outline-rgxp (substring outline-regexp 0 -8))
         (heading-1-regexp
         (format "%s%s%s%s"
                 outline-rgxp
                 "\\{1\\} \\(.*"
                 (if outshine-fontify-whole-heading-line "\n?" "")
                 "\\)"))
        (heading-2-regexp
         (format "%s%s%s%s"
                 outline-rgxp
                 "\\{2\\} \\(.*"
                 (if outshine-fontify-whole-heading-line "\n?" "")
                 "\\)"))
        (heading-3-regexp
         (format "%s%s%s%s"
                 outline-rgxp
                 "\\{3\\} \\(.*"
                 (if outshine-fontify-whole-heading-line "\n?" "")
                 "\\)"))
        (heading-4-regexp
         (format "%s%s%s%s"
                 outline-rgxp
                 "\\{4\\} \\(.*"
                 (if outshine-fontify-whole-heading-line "\n?" "")
                 "\\)"))
        (heading-5-regexp
         (format "%s%s%s%s"
                 outline-rgxp
                 "\\{5\\} \\(.*"
                 (if outshine-fontify-whole-heading-line "\n?" "")
                 "\\)"))
        (heading-6-regexp
         (format "%s%s%s%s"
                 outline-rgxp
                 "\\{6\\} \\(.*"
                 (if outshine-fontify-whole-heading-line "\n?" "")
                 "\\)"))
        (heading-7-regexp
         (format "%s%s%s%s"
                 outline-rgxp
                 "\\{7\\} \\(.*"
                 (if outshine-fontify-whole-heading-line "\n?" "")
                 "\\)"))
        (heading-8-regexp
         (format "%s%s%s%s"
                 outline-rgxp
                 "\\{8\\} \\(.*"
                 (if outshine-fontify-whole-heading-line "\n?" "")
                 "\\)")))
    (font-lock-add-keywords
     nil
     `((,heading-1-regexp 1 'outline-1 t)
       (,heading-2-regexp 1 'outline-2 t)
       (,heading-3-regexp 1 'outline-3 t)
       (,heading-4-regexp 1 'outline-4 t)
       (,heading-5-regexp 1 'outline-5 t)
       (,heading-6-regexp 1 'outline-6 t)
       (,heading-7-regexp 1 'outline-7 t)
       (,heading-8-regexp 1 'outline-8 t)))
    (outshine-font-lock-flush)))


;;;;; Functions for hiding comment-subtrees

(defun outshine-hide-comment-subtrees-in-region (beg end)
  "Re-hide all comment subtrees after a visibility state change."
  (save-excursion
    (let* ((re (concat ":" outshine-comment-tag ":")))
      (goto-char beg)
      (while (re-search-forward re end t)
        (when (outline-on-heading-p t)
          (outshine-flag-subtree t)
          (outline-end-of-subtree))))))

(defun outshine-flag-subtree (flag)
  (save-excursion
    (outline-back-to-heading t)
    (outline-end-of-heading)
    (outline-flag-region (point)
                         (progn (outline-end-of-subtree) (point))
                         flag)))

(defun outshine-hide-comment-subtrees ()
  "Re-hide all comment subtrees after a visibility state change."
  (let ((state outshine-current-buffer-visibility-state))
  (when (and (not outshine-open-comment-trees)
             (not (memq state '(overview folded))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
             (beg (if globalp (point-min) (point)))
             (end (if globalp (point-max)
                    (outline-end-of-subtree))))
        (outshine-hide-comment-subtrees-in-region beg end)
        (goto-char beg)
        (if (looking-at (concat ".*:" outshine-comment-tag ":"))
            (message "%s" (substitute-command-keys
                           "Subtree is tagged as comment and
                           stays closed. Use
                           \\[outshine-force-cycle-comment] to
                           cycle it anyway."))))))))

;; ;; FIXME max-lisp-eval-depth exceeded error when turned on
;; ;; with max-lisp-eval-depth set to 600
;; (add-hook 'outline-view-change-hook
;; 	  'outshine-hide-comment-subtrees)


(defun outshine-comment-region (beg end &optional arg)
       "Use comment-style that always inserts at BOL.
Call `comment-region' with a comment-style that guarantees
   insertion of comment-start markers at beginning-of-line."
       (interactive "r")
       (let ((comment-style
              (if (member comment-style '(indent-or-triple indent))
                  'plain
                comment-style)))
         (comment-region beg end arg)))

;;;;; Hook function

;;;###autoload
(defun outshine-hook-function ()
  "Add this function to outline-minor-mode-hook"
  (outshine-set-outline-regexp-base)
  (outshine-normalize-regexps)
  (let ((out-regexp (outshine-calc-outline-regexp)))
    (outshine-set-local-outline-regexp-and-level
     out-regexp
     'outshine-calc-outline-level
     outshine-outline-heading-end-regexp)
    (setq outline-promotion-headings
          (outshine-make-promotion-headings-list 8))
    ;; imenu preparation
    (and outshine-imenu-show-headlines-p
         (set (make-local-variable
               'outshine-imenu-preliminary-generic-expression)
               `((nil ,(concat out-regexp "\\(.*$\\)") 1)))
	 (if imenu-generic-expression
	     (add-to-list 'imenu-generic-expression
                          (car outshine-imenu-preliminary-generic-expression))
           (setq imenu-generic-expression
                 outshine-imenu-preliminary-generic-expression)))
    (when outshine-startup-folded-p
      (condition-case error-data
          (outline-hide-sublevels 1)
        ('error (message "No outline structure detected"))))
    (when (pcase outshine-fontify
            (`t t)
            (`nil nil)
            ((pred functionp) (funcall outshine-fontify))
            (_ (user-error "Invalid value for variable `outshine-fontify'")))
      (outshine-fontify-headlines out-regexp))))

;; ;; add this to your .emacs
;; (add-hook 'outline-minor-mode-hook 'outshine-hook-function)

;;;;; Additional outline functions
;;;;;; Functions from `outline-magic'

(defun outline-cycle-emulate-tab ()
  "Check if TAB should be emulated at the current position."
  ;; This is called after the check for point in a headline,
  ;; so we can assume we are not in a headline
  (if (and (eq outline-cycle-emulate-tab 'white)
           (save-excursion
             (beginning-of-line 1) (looking-at "[ \t]+$")))
      t
    outline-cycle-emulate-tab))

(defun outline-change-level (delta)
  "Workhorse for `outline-demote' and `outline-promote'."
  (let* ((headlist (outline-headings-list))
         (atom (outline-headings-atom headlist))
         (re (concat "^" outline-regexp))
         (transmode (and transient-mark-mode mark-active))
         beg end)

    ;; Find the boundaries for this operation
    (save-excursion
      (if transmode
          (setq beg (min (point) (mark))
                end (max (point) (mark)))
        (outline-back-to-heading)
        (setq beg (point))
        (outline-end-of-heading)
        (outline-end-of-subtree)
        (setq end (point)))
      (setq beg (move-marker (make-marker) beg)
            end (move-marker (make-marker) end))

      (let (head newhead level newlevel static)

        ;; First a dry run to test if there is any trouble ahead.
        (goto-char beg)
        (while (re-search-forward re end t)
          (outline-change-heading headlist delta atom 'test))

        ;; Now really do replace the headings
        (goto-char beg)
        (while (re-search-forward re end t)
          (outline-change-heading headlist delta atom))))))

(defun outline-headings-list ()
  "Return a list of relevant headings, either a user/mode defined
list, or an alist derived from scanning the buffer."
  (let (headlist)
    (cond
     (outline-promotion-headings
      ;; configured by the user or the mode
      (setq headlist outline-promotion-headings))

     ((and (eq major-mode 'outline-mode) (string= outline-regexp "[*\^L]+"))
      ;; default outline mode with original regexp
      ;; this need special treatment because of the \f in the regexp
      (setq headlist '(("*" . 1) ("**" . 2))))  ; will be extrapolated

     (t ;; Check if the buffer contains a complete set of headings
      (let ((re (concat "^" outline-regexp)) head level)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward re nil t)
            (save-excursion
              (beginning-of-line 1)
              (setq head (outline-cleanup-match (match-string 0))
                    level (funcall outline-level))
              (add-to-list  'headlist (cons head level))))))
      ;; Check for uniqueness of levels in the list
      (let* ((hl headlist) entry level seen nonunique)
        (while (setq entry (car hl))
          (setq hl (cdr hl)
                level (cdr entry))
          (if (and (not (outline-static-level-p level))
                   (member level seen))
              ;; We have two entries for the same level.
              (add-to-list 'nonunique level))
          (add-to-list 'seen level))
        (if nonunique
            (error "Cannot promote/demote: non-unique headings at level %s\nYou may want to configure `outline-promotion-headings'."
                   (mapconcat 'int-to-string nonunique ","))))))
    ;; OK, return the list
    headlist))

(defun outline-change-heading (headlist delta atom &optional test)
  "Change heading just matched by `outline-regexp' by DELTA levels.
HEADLIST can be either an alist ((\"outline-match\" . level)...) or a
straight list like `outline-promotion-headings'. ATOM is a character
if all headlines are composed of a single character.
If TEST is non-nil, just prepare the change and error if there are problems.
TEST nil means, really replace old heading with new one."
  (let* ((head (outline-cleanup-match (match-string 0)))
         (level (save-excursion
                  (beginning-of-line 1)
                  (funcall outline-level)))
         (newhead  ; compute the new head
          (cond
           ((= delta 0) t)
           ((outline-static-level-p level) t)
           ((null headlist) nil)
           ((consp (car headlist))
            ;; The headlist is an association list
            (or (car (rassoc (+ delta level) headlist))
                (and atom
                     (> (+ delta level) 0)
                     (make-string (+ delta level) atom))))
           (t
            ;; The headlist is a straight list - grab the correct element.
            (let* ((l (length headlist))
                   (n1 (- l (length (member head headlist)))) ; index old
                   (n2 (+ delta n1)))                         ; index new
              ;; Careful checking
              (cond
               ((= n1 l) nil)                ; head not found
               ((< n2 0) nil)                ; newlevel too low
               ((>= n2 l) nil)               ; newlevel too high
               ((let* ((tail (nthcdr (min n1 n2) headlist))
                       (nilpos (- (length tail) (length (memq nil tail)))))
                  (< nilpos delta))          ; nil element between old and new
                nil)
               (t (nth n2 headlist))))))))      ; OK, we have a match!
    (if (not newhead)
        (error "Cannot shift level %d heading \"%s\" to level %d"
               level head (+ level delta)))
    (if (and (not test) (stringp newhead))
        (save-excursion
          (beginning-of-line 1)
          (or (looking-at (concat "[ \t]*\\(" (regexp-quote head) "\\)"))
              (error "Please contact maintainer"))
          (replace-match (outline-cleanup-match newhead) t t nil 1)))))

(defun outline-headings-atom (headlist)
  "Use the list created by `outline-headings-list' and check if all
headings are polymers of a single character, e.g. \"*\".
If yes, return this character."
  (if (consp (car headlist))
      ;; this is an alist - it makes sense to check for atomic structure
      (let ((re (concat "\\`"
                        (regexp-quote (substring (car (car headlist)) 0 1))
                        "+\\'")))
        (if (not (delq nil (mapcar (lambda (x) (not (string-match re (car x))))
                                   headlist)))
            (string-to-char (car (car headlist)))))))

(defun outline-cleanup-match (s)
  "Remove text properties and start/end whitespace from a string."
  (set-text-properties 1 (length s) nil s)
  (save-match-data
    (if (string-match "^[ \t]+" s) (setq s (replace-match "" t t s)))
    (if (string-match "[ \t]+$" s) (setq s (replace-match "" t t s))))
  s)

(defun outline-static-level-p (level)
  "Test if a level should not be changed by level promotion/demotion."
  (>= level 1000))


(defun outline-hide-sublevels (keep-levels)
  "Hide everything except the first KEEP-LEVEL headers."
  (interactive "p")
  (if (< keep-levels 1)
      (error "Must keep at least one level of headers"))
  (setq keep-levels (1- keep-levels))
  (save-excursion
    (goto-char (point-min))
    ;; Skip the prelude, if any.
    (unless (outline-on-heading-p t) (outline-next-heading))
    (hide-subtree)
    (show-children keep-levels)
    (condition-case err
        (while (outline-get-next-sibling)
          (hide-subtree)
          (show-children keep-levels))
      (error nil))))

(defun outline-hide-other ()
  "Hide everything except for the current body and the parent headings."
  (interactive)
  (outline-hide-sublevels 1)
  (let ((last (point))
        (pos (point)))
    (while (save-excursion
             (and (re-search-backward "[\n\r]" nil t)
                  (eq (following-char) ?\r)))
      (save-excursion
        (beginning-of-line)
        (if (eq last (point))
            (progn
              (outline-next-heading)
              (outline-flag-region last (point) ?\n))
          (show-children)
          (setq last (point)))))))

;;;;;; Commands from `outline-magic'

(defun outline-next-line ()
  "Forward line, but mover over invisible line ends.
Essentially a much simplified version of `next-line'."
  (interactive)
  (beginning-of-line 2)
  (while (and (not (eobp))
              (get-char-property (1- (point)) 'invisible))
    (beginning-of-line 2)))

(defun outline-move-subtree-up (&optional arg)
  "Move the currrent subtree up past ARG headlines of the same level."
  (interactive "p")
  (let ((headers (or arg 1)))
    (outline-move-subtree-down (- headers))))

(defun outline-move-subtree-down (&optional arg)
  "Move the currrent subtree down past ARG headlines of the same level."
  (interactive "p")
  (let* ((headers (or arg 1))
        (re (concat "^" outline-regexp))
        (movfunc (if (> headers 0) 'outline-get-next-sibling
                   'outline-get-last-sibling))
        (ins-point (make-marker))
        (cnt (abs headers))
        beg end txt)
    ;; Select the tree
    (outline-back-to-heading)
    (setq beg (point))
    (outline-end-of-subtree)
    (if (= (char-after) ?\n) (forward-char 1))
    (setq end (point))
    ;; Find insertion point, with error handling
    (goto-char beg)
    (while (> cnt 0)
      (or (funcall movfunc)
          (progn (goto-char beg)
                 (error "Cannot move past superior level")))
      (setq cnt (1- cnt)))
    (if (> headers 0)
        ;; Moving forward - still need to move over subtree
        (progn (outline-end-of-subtree)
               (if (= (char-after) ?\n) (forward-char 1))))
    (move-marker ins-point (point))
    (setq txt (buffer-substring beg end))
    (delete-region beg end)
    (insert txt)
    (goto-char ins-point)
    (move-marker ins-point nil)))

(defun outline-promote (&optional arg)
  "Decrease the level of an outline-structure by ARG levels.
When the region is active in transient-mark-mode, all headlines in the
region are changed.  Otherwise the current subtree is targeted. Note that
after each application of the command the scope of \"current subtree\"
may have changed."
  (interactive "p")
  (let ((delta (or arg 1)))
    (outline-change-level (- delta))))

(defun outline-demote (&optional arg)
  "Increase the level of an outline-structure by ARG levels.
When the region is active in transient-mark-mode, all headlines in the
region are changed.  Otherwise the current subtree is targeted. Note that
after each application of the command the scope of \"current subtree\"
may have changed."
  (interactive "p")
  (let ((delta (or arg 1)))
    (outline-change-level delta)))

(defun outline-cycle (&optional arg)
  "Visibility cycling for outline(-minor)-mode.

- When point is at the beginning of the buffer, or when called with a
  C-u prefix argument, rotate the entire buffer through 3 states:
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.

- When point is at the beginning of a headline, rotate the subtree started
  by this line through 3 different states:
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.  From
               this state, you can move to one of the children and
               zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.

- When point is not at the beginning of a headline, execute
  `indent-relative', like TAB normally does."
  (interactive "P")
  (setq deactivate-mark t)
  (cond

   ((equal arg '(4))
    ;; Run `outline-cycle' as if at the top of the buffer.
    (let ((outshine-org-style-global-cycling-at-bob-p nil)
          (current-prefix-arg nil))
    (save-excursion
      (goto-char (point-min))
      (outline-cycle nil))))

   (t
    (cond
     ;; Beginning of buffer: Global cycling
     ((or
       ;; outline-magic style behaviour
       (and
        (bobp)
        (not outshine-org-style-global-cycling-at-bob-p))
       ;; org-mode style behaviour
       (and
        (bobp)
        (not (outline-on-heading-p))
        outshine-org-style-global-cycling-at-bob-p))
      (cond
       ((eq last-command 'outline-cycle-overview)
        ;; We just created the overview - now do table of contents
        ;; This can be slow in very large buffers, so indicate action
        (unless outshine-cycle-silently
          (message "CONTENTS..."))
        (save-excursion
          ;; Visit all headings and show their offspring
          (goto-char (point-max))
          (catch 'exit
            (while (and (progn (condition-case nil
                                   (outline-previous-visible-heading 1)
                                 (error (goto-char (point-min))))
                               t)
                        (looking-at outline-regexp))
              (show-branches)
              (if (bobp) (throw 'exit nil))))
          (unless outshine-cycle-silently
            (message "CONTENTS...done")))
        (setq
         this-command 'outline-cycle-toc
         outshine-current-buffer-visibility-state 'contents))
       ((eq last-command 'outline-cycle-toc)
        ;; We just showed the table of contents - now show everything
        (show-all)
        (unless outshine-cycle-silently
          (message "SHOW ALL"))
        (setq
         this-command 'outline-cycle-showall
         outshine-current-buffer-visibility-state 'all))
       (t
        ;; Default action: go to overview
        ;; (hide-sublevels 1)
        (let ((toplevel
               (cond
                (current-prefix-arg
                 (prefix-numeric-value current-prefix-arg))
                ((save-excursion
                   (beginning-of-line)
                   (looking-at outline-regexp))
                 (max 1 (funcall outline-level)))
                (t 1))))
          (hide-sublevels toplevel))
        (unless outshine-cycle-silently
          (message "OVERVIEW"))
        (setq
         this-command 'outline-cycle-overview
         outshine-current-buffer-visibility-state 'overview))))

     ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
      ;; At a heading: rotate between three different views
      (outline-back-to-heading)
      (let ((goal-column 0) beg eoh eol eos)
        ;; First, some boundaries
        (save-excursion
          (outline-back-to-heading)           (setq beg (point))
          (save-excursion (outline-next-line) (setq eol (point)))
          (outline-end-of-heading)            (setq eoh (point))
          (outline-end-of-subtree)            (setq eos (point)))
        ;; Find out what to do next and set `this-command'
        (cond
         ((= eos eoh)
          ;; Nothing is hidden behind this heading
          (unless outshine-cycle-silently
            (message "EMPTY ENTRY")))
         ((>= eol eos)
          ;; Entire subtree is hidden in one line: open it
          (show-entry)
          (show-children)
          (unless outshine-cycle-silently
            (message "CHILDREN"))
          (setq
           this-command 'outline-cycle-children))
           ;; outshine-current-buffer-visibility-state 'children))
         ((eq last-command 'outline-cycle-children)
          ;; We just showed the children, now show everything.
          (show-subtree)
          (unless outshine-cycle-silently
            (message "SUBTREE")))
         (t
          ;; Default action: hide the subtree.
          (hide-subtree)
          (unless outshine-cycle-silently
            (message "FOLDED"))))))

     ;; TAB emulation
     ((outline-cycle-emulate-tab)
      (indent-relative))

     (t
      ;; Not at a headline: Do indent-relative
      (outline-back-to-heading))))))

(defun outshine-cycle-buffer ()
  "Cycle the visibility state of buffer."
  (interactive)
  (outline-cycle '(4)))

(defun outshine-toggle-silent-cycling (&optional arg)
  "Toggle silent cycling between visibility states.

  When silent cycling is off, visibility state-change messages are
  written to stdout (i.e. the *Messages* buffer), otherwise these
  messages are suppressed. With prefix argument ARG, cycle silently
  if ARG is positive, otherwise write state-change messages."
  (interactive "P")
  (setq outshine-cycle-silently
        (if (null arg)
            (not outshine-cycle-silently)
          (> (prefix-numeric-value arg) 0)))
  (message "Silent visibility cycling %s"
           (if outshine-cycle-silently "enabled" "disabled")))



;;;;;; Commands from `outline-mode-easy-bindings'

;; Copied from: http://emacswiki.org/emacs/OutlineMinorMode

(defun outline-body-p ()
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (and (not (eobp))
         (progn (forward-char 1)
                (not (outline-on-heading-p))))))

(defun outline-body-visible-p ()
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (not (outline-invisible-p))))

(defun outline-subheadings-p ()
  (save-excursion
    (outline-back-to-heading)
    (let ((level (funcall outline-level)))
      (outline-next-heading)
      (and (not (eobp))
           (< level (funcall outline-level))))))

(defun outline-subheadings-visible-p ()
  (interactive)
  (save-excursion
    (outline-next-heading)
    (not (outline-invisible-p))))

(defun outline-hide-more ()
  (interactive)
  (when (outline-on-heading-p)
    (cond ((and (outline-body-p)
                (outline-body-visible-p))
           (hide-entry)
           (hide-leaves))
          (t
           (hide-subtree)))))

(defun outline-show-more ()
  (interactive)
  (when (outline-on-heading-p)
    (cond ((and (outline-subheadings-p)
                (not (outline-subheadings-visible-p)))
           (show-children))
          ((and (not (outline-subheadings-p))
                (not (outline-body-visible-p)))
           (show-subtree))
          ((and (outline-body-p)
                (not (outline-body-visible-p)))
           (show-entry))
          (t
           (show-subtree)))))

;;;;; Hidden-line-cookies commands

(defun outshine-show-hidden-lines-cookies ()
  "Show hidden-lines cookies for all visible and folded headlines."
  (interactive)
  (if outshine-show-hidden-lines-cookies-p
      (outshine-write-hidden-lines-cookies)
    (if (not (y-or-n-p "Activate hidden-lines cookies "))
        (message "Unable to show hidden-lines cookies - deactivated.")
      (outshine-toggle-hidden-lines-cookies-activation)
      (outshine-write-hidden-lines-cookies)))
  (setq outshine-hidden-lines-cookies-on-p 1))

(defun outshine-hide-hidden-lines-cookies ()
  "Delete all hidden-lines cookies."
  (interactive)
  (let* ((base-buf (point-marker))
         (indirect-buf-name
          (generate-new-buffer-name
           (buffer-name (marker-buffer base-buf)))))
    (unless outshine-show-hidden-lines-cookies-p
      (setq outshine-show-hidden-lines-cookies-p 1))
    (clone-indirect-buffer indirect-buf-name nil 'NORECORD)
    (save-excursion
      (switch-to-buffer indirect-buf-name)
      (show-all)
      (let ((indirect-buf (point-marker)))
        (outshine-write-hidden-lines-cookies)
        (switch-to-buffer (marker-buffer base-buf))
        (kill-buffer (marker-buffer indirect-buf))
        (set-marker indirect-buf nil))
      (set-marker base-buf nil)))
  (setq outshine-hidden-lines-cookies-on-p nil))

(defun outshine-toggle-hidden-lines-cookies-activation ()
  "Toggles activation of hidden-lines cookies."
  (interactive)
  (if outshine-show-hidden-lines-cookies-p
      (progn
        (setq outshine-show-hidden-lines-cookies-p nil)
        (setq outshine-hidden-lines-cookies-on-p nil)
        (message "hidden-lines cookies are deactivated now"))
    (setq outshine-show-hidden-lines-cookies-p 1)
    (message "hidden-lines cookies are activated now")))

(defun outshine-toggle-hidden-lines-cookies ()
  "Toggles status of hidden-lines cookies between shown and hidden."
  (interactive)
  (if outshine-hidden-lines-cookies-on-p
      (outshine-hide-hidden-lines-cookies)
    (outshine-show-hidden-lines-cookies)))

;;;;; Hide comment-subtrees

(defun outshine-insert-comment-subtree (&optional arg)
  "Insert new subtree that is tagged as comment."
    (interactive "P")
    (outshine-insert-heading)
    (save-excursion
      (insert
       (concat "  :" outshine-comment-tag ":"))))

(defun outshine-toggle-subtree-comment-status (&optional arg)
  "Tag (or untag) subtree at point with `outshine-comment-tag'.

Unless point is on a heading, this function acts on the previous
visible heading when ARG is non-nil, otherwise on the previous
heading."
  (interactive "P")
  (let* ((com-end-p
          (and
           outshine-normalized-comment-end
           (> (length outshine-normalized-comment-end) 0)))
         (comtag (concat ":" outshine-comment-tag ":"))
         (comtag-rgxp
          (if com-end-p
              (concat comtag
                      " *"
                      (regexp-quote
                       outshine-normalized-comment-end)
                      " *")
            (concat comtag " *"))))
    (unless (outline-on-heading-p)
      (if arg
          (outline-previous-visible-heading 1)
        (outline-previous-heading)))
    (end-of-line)
    (cond
     ((looking-back comtag-rgxp)
      (let ((start (match-beginning 0)))
        (delete-region (1- start) (+ start (length comtag)))))
     ((and com-end-p
           (looking-back
            (concat
             (regexp-quote outshine-normalized-comment-end) " *")))
      (goto-char (match-beginning 0))
       (if (looking-back " ")
           (insert (concat comtag " "))
         (insert (concat " " comtag))))
     (t (if (looking-back " ")
          (insert comtag)
         (insert (concat " " comtag)))))))

;; Cycle comment subtrees anyway
(defun outshine-force-cycle-comment ()
  "Cycle subtree even if it comment."
  (interactive)
  (setq this-command 'outline-cycle)
  (let ((outshine-open-comment-trees t))
    (call-interactively 'outline-cycle)))


(defun outshine-self-insert-command (N)
  "Like `self-insert-command', use overwrite-mode for whitespace in tables.
If the cursor is in a table looking at whitespace, the whitespace is
overwritten, and the table is not marked as requiring realignment."
  (interactive "p")
  ;; (outshine-check-before-invisible-edit 'insert)
  (self-insert-command N)
  (if outshine-self-insert-cluster-for-undo
      (if (not (eq last-command 'outshine-self-insert-command))
          (setq outshine-self-insert-command-undo-counter 1)
        (if (>= outshine-self-insert-command-undo-counter 20)
            (setq outshine-self-insert-command-undo-counter 1)
          (and (> outshine-self-insert-command-undo-counter 0)
               buffer-undo-list (listp buffer-undo-list)
               (not (cadr buffer-undo-list)) ; remove nil entry
               (setcdr buffer-undo-list (cddr buffer-undo-list)))
          (setq outshine-self-insert-command-undo-counter
                (1+ outshine-self-insert-command-undo-counter))))))

;; comply with `delete-selection-mode' and `electric-pair-mode'
(put 'outshine-self-insert-command 'delete-selection
     (lambda ()
       (not (run-hook-with-args-until-success
             'self-insert-uses-region-functions))))

;; trigger company idle completion like namesake command
(put 'outshine-self-insert-command 'company-begin t)

;; trigger eldoc (elisp help in echo area), like `self-insert-command'
(with-eval-after-load 'eldoc
  (eldoc-add-command 'outshine-self-insert-command))

;;;;; Other Commands

(defun outshine-eval-lisp-subtree ()
  "Mark subtree at point and call `eval-region' on it."
  (interactive)
  (save-excursion
    (outline-mark-subtree)
    (call-interactively 'eval-region)))

(defun outshine-comment-subtree-content ()
  "Mark subtree at point and call `comment-dwim' on its content."
  (interactive)
  (save-excursion
    (outline-mark-subtree)
    (forward-line)
    (call-interactively 'comment-dwim)))

(defun outshine-narrow-to-subtree ()
  "Narrow buffer to subtree at point."
  (interactive)
  (if (outline-on-heading-p)
      (progn
        (outline-mark-subtree)
        (and
         (use-region-p)
         (narrow-to-region (region-beginning) (region-end)))
        (deactivate-mark))
    (message "Not at headline, cannot narrow to subtree")))


;;;;; Overridden outline commands

;; overriding 'outline-insert-heading'
;; copied and adapted form outline.el, taking into account modes
;; with 'comment-end' defined (as non-empty string).

(defun outshine-insert-heading ()
  "Insert a new heading at same depth at point.
This function takes `comment-end' into account."
  (interactive)
  (let* ((head-with-prop
          (save-excursion
            (condition-case nil
                (outline-back-to-heading)
              (error (outline-next-heading)))
            (if (eobp)
                (or (caar outline-heading-alist) "")
              (match-string 0))))
         (head (substring-no-properties head-with-prop))
         (com-end-p))
    (unless (or (string-match "[ \t]\\'" head)
                (not (string-match (concat "\\`\\(?:" outline-regexp "\\)")
                                   (concat head " "))))
      (setq head (concat head " ")))
    (unless (or (not comment-end) (string-equal "" comment-end))
      (setq head (concat head " " outshine-normalized-comment-end))
      (setq com-end-p t))
    (unless (bolp) (end-of-line) (newline))
    (insert head)
    (unless (eolp)
      (save-excursion (newline-and-indent)))
    (and com-end-p
         (re-search-backward outshine-normalized-comment-end)
         (forward-char -1))
    (run-hooks 'outline-insert-heading-hook)))

(defun outshine-imenu (&optional PREFER-IMENU-P)
  "Convenience function for calling imenu/idomenu from outshine."
  (interactive "P")
  (or outshine-imenu-default-generic-expression
      (setq outshine-imenu-default-generic-expression
            outshine-imenu-preliminary-generic-expression))
  (let* ((imenu-generic-expression
          outshine-imenu-default-generic-expression)
         (imenu-prev-index-position-function nil)
         (imenu-extract-index-name-function nil)
         (imenu-auto-rescan t)
         (imenu-auto-rescan-maxout 360000))
    ;; prefer idomenu
    (funcall 'imenu
               (imenu-choose-buffer-index
                "Headline: "))))

;;;;; Advertise Bindings

;; (put 'outshine-insert-heading :advertised-binding [M-ret])
;; (put 'outline-cycle :advertised-binding [?\t])
;; (put 'outshine-cycle-buffer :advertised-binding [backtab])
;; (put 'outline-promote :advertised-binding [M-S-left])
;; (put 'outline-demote :advertised-binding [M-S-right])
;; (put 'outline-move-subtree-up :advertised-binding [M-S-up])
;; (put 'outline-move-subtree-down :advertised-binding [M-S-down])
;; (put 'outline-hide-more :advertised-binding [M-left])
;; (put 'outline-show-more :advertised-binding [M-right])
;; (put 'outline-next-visible-header :advertised-binding [M-down])
;; (put 'outline-previous-visible-header :advertised-binding [M-up])
;; (put 'show-all :advertised-binding [?\M-# \?M-a])
;; (put 'outline-up-heading :advertised-binding [?\M-# ?\M-u])


(run-hooks 'outshine-hook)



(provide 'outshine)
;;; outshine.el ends here

;; Local Variables:
;; coding: utf-8
;; ispell-local-dictionary: "en_US"
;; indent-tabs-mode: nil
;; End:
