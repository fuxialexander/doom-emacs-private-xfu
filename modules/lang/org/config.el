;;; lang/org/config.el -*- lexical-binding: t; -*-

(defvar +org-dir (expand-file-name "~/Dropbox/org/")
  "The directory where org files are kept.")

;; Ensure ELPA org is prioritized above built-in org.
;; (when-let* ((path (locate-library "org" nil doom--package-load-path)))
;;   (setq load-path (delete path load-path))
;;   (push (file-name-directory path) load-path))



;; Sub-modules
(if (featurep! +todo)   (load! +todo))
(if (featurep! +attach)  (load! +attach))
(if (featurep! +babel)   (load! +babel))
(if (featurep! +latex)   (load! +latex))
(if (featurep! +capture) (load! +capture))
(if (featurep! +export)  (load! +export))
(if (featurep! +present) (load! +present))
;; TODO (if (featurep! +publish) (load! +publish))


;;
;; Plugins
;;

(def-package! toc-org
  :commands toc-org-enable)

;; (def-package! org-bullets
;;   :commands org-bullets-mode
;;   :config
;;   (setq org-bullets-bullet-list '("#" "#" "#" "#" "#" "#" "#" "#")))

(def-package! org-brain
  :after org
  :init
  (setq org-brain-path "~/Dropbox/org")
  (push 'org-agenda-mode evil-snipe-disabled-modes)
  ;; (add-hook 'org-agenda-mode-hook #'(lambda () (evil-vimish-fold-mode -1)))
  (set! :evil-state 'org-brain-visualize-mode 'normal)
  :config
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
        org-brain-title-max-length 30)


  (map!
   (:map org-brain-visualize-mode-map
     :n "a" 'org-brain-visualize-attach
     :n "b" 'org-brain-visualize-back
     :n "c" 'org-brain-add-child
     :n "C" 'org-brain-remove-child
     :n "p" 'org-brain-add-parent
     :n "P" 'org-brain-remove-parent
     :n "f" 'org-brain-add-friendship
     :n "F" 'org-brain-remove-friendship
     :n "d" 'org-brain-delete-entry
     :n "g" 'revert-buffer
     :n "_" 'org-brain-new-child
     :n ";" 'org-brain-set-tags
     :n "j" 'forward-button
     :n "k" 'backward-button
     :n "l" 'org-brain-add-resource
     :n "L" 'org-brain-visualize-paste-resource
     :n "t" 'org-brain-set-title
     :n "m" 'org-brain-pin
     :n "o" 'ace-link-woman
     :n "q" 'org-brain-visualize-quit
     :n "r" 'org-brain-visualize-random
     :n "R" 'org-brain-visualize-wander
     :n "s" 'org-brain-visualize
     :n "S" 'org-brain-goto
     :n [tab] 'org-brain-goto-current
     )))

(def-package! org-web-tools
  :after org)

;;
;; Bootstrap
;;
(add-hook! 'org-load-hook
  #'(+org|setup-ui
     +org|setup-keybinds
     +org|setup-hacks
     +org|setup-overrides))


(add-hook! 'org-mode-hook
  #'(doom|disable-line-numbers  ; no line numbers
     ;; org-bullets-mode           ; "prettier" bullets
     org-indent-mode            ; margin-based indentation
     toc-org-enable             ; auto-table of contents
     visual-line-mode           ; line wrapping

     +org|enable-auto-reformat-tables
     +org|enable-auto-update-cookies
     +org|smartparens-compatibility-config
     +org|unfold-to-2nd-level-or-point
     +org|show-paren-mode-compatibility
     ))





(when (featurep 'org)
  (run-hooks 'org-load-hook))


;;
;; `org-mode' hooks
;;

(defun +org|unfold-to-2nd-level-or-point ()
  "My version of the 'overview' #+STARTUP option: expand first-level headings.
Expands the first level, but no further. If point was left somewhere deeper,
unfold to point on startup."
  (unless org-agenda-inhibit-startup
    (when (eq org-startup-folded t)
      (outline-hide-sublevels 2))
    (when (outline-invisible-p)
      (ignore-errors
        (save-excursion
          (outline-previous-visible-heading 1)
          (org-show-subtree))))))

(defun +org|smartparens-compatibility-config ()
  "Instruct `smartparens' not to impose itself in org-mode."
  (defun +org-sp-point-in-checkbox-p (_id action _context)
    (when (eq action 'insert)
      (sp--looking-at-p "\\s-*]")))

  ;; make delimiter auto-closing a little more conservative
  (after! smartparens
    (sp-with-modes 'org-mode
      (sp-local-pair "\\[" "\\]")
      (sp-local-pair "\\(" "\\)")
      (sp-local-pair "*" nil :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-at-bol-p))
      (sp-local-pair "_" nil :unless '(sp-point-after-word-p sp-point-before-word-p))
      (sp-local-pair "/" nil :unless '(sp-point-after-word-p sp-point-before-word-p +org-sp-point-in-checkbox-p))
      (sp-local-pair "~" nil :unless '(sp-point-after-word-p sp-point-before-word-p))
      (sp-local-pair "=" nil :unless '(sp-point-after-word-p sp-point-before-word-p)))))

(defun +org|enable-auto-reformat-tables ()
  "Realign tables exiting insert mode (`evil-mode')."
  (when (featurep 'evil)
    (add-hook 'evil-insert-state-exit-hook #'+org|realign-table-maybe nil t)))

(defun +org|enable-auto-update-cookies ()
  "Update statistics cookies when saving or exiting insert mode (`evil-mode')."
  (when (featurep 'evil)
    (add-hook 'evil-insert-state-exit-hook #'+org|update-cookies nil t))
  (add-hook 'before-save-hook #'+org|update-cookies nil t))

(defun +org|show-paren-mode-compatibility ()
  "`show-paren-mode' causes flickering with indentation margins made by
`org-indent-mode', so we simply turn off show-paren-mode altogether."
  (set (make-local-variable 'show-paren-mode) nil))


;;
;; `org-load' hooks
;;

(defun +org|setup-ui ()
  "Configures the UI for `org-mode'."
  (setq-default org-adapt-indentation nil
                org-blank-before-new-entry nil
                org-cycle-include-plain-lists t
                org-cycle-separator-lines 1
                org-entities-user '(("flat"  "\\flat" nil "" "" "266D" "♭") ("sharp" "\\sharp" nil "" "" "266F" "♯"))
                org-fontify-done-headline t
                org-fontify-quote-and-verse-blocks t
                org-fontify-whole-heading-line t
                org-footnote-auto-label 'plain
                org-hidden-keywords nil
                org-hide-emphasis-markers nil
                org-hide-leading-stars nil
                org-hide-leading-stars-before-indent-mode nil
                org-id-link-to-org-use-id t
                org-id-locations-file (concat +org-dir ".org-id-locations")
                org-id-track-globally t
                org-image-actual-width nil
                org-indent-indentation-per-level 2
                org-indent-mode-turns-on-hiding-stars t
                org-pretty-entities nil
                org-pretty-entities-include-sub-superscripts t
                org-priority-faces
                `((?a . ,(face-foreground 'error))
                  (?b . ,(face-foreground 'warning))
                  (?c . ,(face-foreground 'success)))
                org-startup-folded t
                org-startup-indented t
                org-startup-with-inline-images nil
                org-tags-column 0
                org-use-sub-superscripts '{}
                outline-blank-line t)

  (org-link-set-parameters
   "org"
   :complete (lambda () (+org-link-read-file "org" +org-dir))
   :follow   (lambda (link) (find-file (expand-file-name link +org-dir)))
   :face     (lambda (link)
               (if (file-exists-p (expand-file-name link +org-dir))
                   'org-link
                 'error))))

(defun +org|setup-keybinds ()
  (map!
   (:after org
     :map org-mode-map
     "RET" #'org-return-indent
     "M-o" #'org-open-at-point
     "M-i" #'org-insert-last-stored-link
     "M-I" #'org-insert-link
     "s-p" #'org-ref-ivy-insert-cite-link
     :n  "RET" #'+org/dwim-at-point
     :n  [tab]     #'+org/toggle-fold
     :n  "t"       #'org-todo
     :n  "T"       #'org-insert-todo-heading-respect-content
     :i  [tab]     #'+org/indent-or-next-field-or-yas-expand
     :i  [S-tab]   #'+org/dedent-or-prev-field

     ;; "C-c C-S-l" #'+org/remove-link
     ;;                          :n "C-c C-i" #'org-toggle-inline-images
     (:localleader
       :desc "Schedule"          :n "s"                  #'org-schedule
       :desc "Math"              :n "m"                  #'+org-toggle-math
       :desc "Remove link"       :n "L"                  #'+org/remove-link
       :desc "Deadline"          :n "d"                  #'org-deadline
       :desc "C-c C-c"           :n doom-localleader-key #'org-ctrl-c-ctrl-c
       :desc "Edit Special"      :n "'"                  #'org-edit-special
       :desc "Effort"            :n "e"                  #'org-set-effort
       :desc "TODO"              :n "t"                  #'org-todo
       :desc "Export"            :n [tab]                #'org-export-dispatch
       :desc "Clocking Effort"   :n "E"                  #'org-clock-modify-effort-estimate
       :desc "Property"          :n "p"                  #'org-set-property
       :desc "Clock-in"          :n "i"                  #'org-clock-in
       :desc "Clock-out"         :n "o"                  #'org-clock-out
       :desc "Narrow to Subtree" :n "n"                  #'org-narrow-to-subtree
       :desc "Narrow to Element" :n "N"                  #'org-narrow-to-element
       :desc "Widen"             :n "w"                  #'widen
       :desc "Toggle heading"    :n "h"                  #'org-toggle-heading
       :desc "Archive Subtree"   :n "A"                  #'org-archive-subtree
       :desc "Toggle Archive"    :n "a"                  #'org-toggle-archive-tag
       )

     ;; :i  "RET" #'evil-org-return
     :ni [s-return]   (λ! (+org/insert-item 'below))
     :ni [C-s-return] (λ! (+org/insert-item 'above))

     :m  "]]"  (λ! (org-forward-heading-same-level nil) (org-beginning-of-line))
     :m  "[["  (λ! (org-backward-heading-same-level nil) (org-beginning-of-line))

     ;; Fix code-folding keybindings
     :n  "za"  #'+org/toggle-fold
     :n  "zA"  #'org-shifttab
     :n  "zc"  #'outline-hide-subtree
     :n  "zC"  (λ! (outline-hide-sublevels 1))
     :n  "zd"  (lambda (&optional arg) (interactive "p") (outline-hide-sublevels (or arg 3)))
     :n  "zm"  (λ! (outline-hide-sublevels 1))
     :n  "zo"  #'outline-show-subtree
     :n  "zO"  #'outline-show-all
     :n  "zr"  #'outline-show-all
     )
   (:after org-agenda
     (:map org-agenda-mode-map
       :nm "C-k"      #'evil-window-up
       :nm "C-j"      #'evil-window-down
       :nm "C-h"      #'evil-window-left
       :nm "C-l"      #'evil-window-right
       :nm "<escape>" #'org-agenda-Quit
       :nm "q" #'org-agenda-Quit

   	   :nm "<C-up>"   #'org-clock-convenience-timestamp-up
   	   :nm "<C-down>" #'org-clock-convenience-timestamp-down
   	   :nm "s-o"      #'org-clock-convenience-fill-gap
   	   :nm "s-e"      #'org-clock-convenience-fill-gap-both

       :nm "\\"       #'ace-window
       :nm "t"        #'org-agenda-todo
       :nm "p"        #'org-set-property
       :nm "r"        #'org-agenda-redo
       :nm "e"        #'org-agenda-set-effort
       :nm "h"        #'org-habit-toggle-habits
       :nm "l"        #'org-agenda-log-mode
       :nm "D"        #'org-agenda-toggle-diary
       :nm "G"        #'org-agenda-toggle-time-grid
       :nm ";"        #'counsel-org-tag-agenda
       :nm "s-j"      #'counsel-org-goto-all

       :nm "i"        #'org-agenda-clock-in
       :nm "o"        #'org-agenda-clock-out

       :nm "<tab>"        #'org-agenda-goto

       :nm "J"        #'org-agenda-later
       :nm "K"        #'org-agenda-earlier

       :nm "C"        #'org-agenda-capture
       :nm "m"        #'org-agenda-bulk-mark
       :nm "u"        #'org-agenda-bulk-unmark
       :nm "U"        #'org-agenda-bulk-unmark-all

       :nm "f"        #'+org@org-agenda-filter/body

       :nm "-"        #'org-agenda-manipulate-query-subtract
       :nm "="        #'org-agenda-manipulate-query-add
       :nm "_"        #'org-agenda-manipulate-query-subtract-re
       :nm "$"        #'org-agenda-manipulate-query-add-re

       :nm "d"        #'org-agenda-deadline
       :nm "s"        #'org-agenda-schedule

       :nm "z"        #'org-agenda-view-mode-dispatch
       :nm "S"        #'org-save-all-org-buffers))
   (:after org-src
     (:map org-src-mode-map
       (:localleader
         :desc "Finish" :n ","  #'org-edit-src-exit
         :desc "Abort"  :n "k"  #'org-edit-src-abort
         )))
   (:after org-capture
     (:map org-capture-mode-map
         "C-c C-c" nil
         "C-c C-k" nil
         "C-c C-w" nil
       (:localleader
         :desc "Finish" :n "," #'org-capture-finalize
         :desc "Refile" :n "r" #'org-capture-refile
         :desc "Abort"  :n "k" #'org-capture-kill
         )))))

(defun +org|setup-hacks ()
  "Getting org to behave."
  ;; Don't open separate windows
  (map-put org-link-frame-setup 'file 'find-file)

  ;; (defun +org|cleanup-agenda-files ()
  ;;   "Close leftover agenda buffers after they've been indexed by org-agenda."
  ;;   (cl-loop for file in org-agenda-files
  ;;            for buf = (get-file-buffer file)
  ;;            if (and file (not (get-buffer-window buf)))
  ;;            do (kill-buffer buf)))
  ;; (add-hook 'org-agenda-finalize-hook #'+org|cleanup-agenda-files)

  ;; Let OS decide what to do with files when opened
  (setq org-file-apps
        `(("\\.org$" . emacs)
          (t . ,(cond (IS-MAC "open \"%s\"")
                      (IS-LINUX "xdg-open \"%s\"")))))

  (defun +org|remove-occur-highlights ()
    "Remove org occur highlights on ESC in normal mode."
    (when (and (derived-mode-p 'org-mode)
               org-occur-highlights)
      (org-remove-occur-highlights)))
  (add-hook 'doom-escape-hook #'+org|remove-occur-highlights)

  (after! recentf
    ;; Don't clobber recentf with agenda files
    (defun +org-is-agenda-file (filename)
      (cl-find (file-truename filename) org-agenda-files
               :key #'file-truename
               :test #'equal))
    (push #'+org-is-agenda-file recentf-exclude)))

(defun +org|setup-overrides ()
  (defun my-org-add-ids-to-headlines-in-file ()
    "Add CUSTOM_ID properties to all headlines in the current file"
    (interactive)
    (unless
        (or
         (string-equal (buffer-name) "cal.org")
         (string-equal default-directory "/Users/xfu/Source/playground/gatsby-orga/src/pages/")
         (string-equal (buffer-name) "cal_kevin.org"))
      (save-excursion
        (widen)
        (goto-char (point-min))
        (org-map-entries 'org-id-get-create))))
  (add-hook 'org-mode-hook (lambda () (add-hook 'before-save-hook 'my-org-add-ids-to-headlines-in-file nil 'local)))

  (defun org-refile-get-targets (&optional default-buffer)
    "Produce a table with refile targets."
    (let ((case-fold-search nil)
          ;; otherwise org confuses "TODO" as a kw and "Todo" as a word
          (entries (or org-refile-targets '((nil . (:level . 1)))))
          targets tgs files desc descre)
      (message "Getting targets...")
      (with-current-buffer (or default-buffer (current-buffer))
        (dolist (entry entries)
          (setq files (car entry) desc (cdr entry))
          (cond
           ((null files) (setq files (list (current-buffer))))
           ((eq files 'org-agenda-files)
            (setq files (org-agenda-files 'unrestricted)))
           ((and (symbolp files) (fboundp files))
            (setq files (funcall files)))
           ((and (symbolp files) (boundp files))
            (setq files (symbol-value files))))
          (when (stringp files) (setq files (list files)))
          (cond
           ((eq (car desc) :tag)
            (setq descre (concat "^\\*+[ \t]+.*?:" (regexp-quote (cdr desc)) ":")))
           ((eq (car desc) :todo)
            (setq descre (concat "^\\*+[ \t]+" (regexp-quote (cdr desc)) "[ \t]")))
           ((eq (car desc) :regexp)
            (setq descre (cdr desc)))
           ((eq (car desc) :level)
            (setq descre (concat "^\\*\\{" (number-to-string
                                            (if org-odd-levels-only
                                                (1- (* 2 (cdr desc)))
                                              (cdr desc)))
                                 "\\}[ \t]")))
           ((eq (car desc) :maxlevel)
            (setq descre (concat "^\\*\\{1," (number-to-string
                                              (if org-odd-levels-only
                                                  (1- (* 2 (cdr desc)))
                                                (cdr desc)))
                                 "\\}[ \t]")))
           (t (error "Bad refiling target description %s" desc)))
          (dolist (f files)
            (with-current-buffer (if (bufferp f) f (org-get-agenda-file-buffer f))
              (or
               (setq tgs (org-refile-cache-get (buffer-file-name) descre))
               (progn
                 (when (bufferp f)
                   (setq f (buffer-file-name (buffer-base-buffer f))))
                 (setq f (and f (expand-file-name f)))
                 (when (eq org-refile-use-outline-path 'file)
                   (push (list (file-name-nondirectory f) f nil nil) tgs))
                 (org-with-wide-buffer
                  (goto-char (point-min))
                  (setq org-outline-path-cache nil)
                  (while (re-search-forward descre nil t)
                    (beginning-of-line)
                    (let ((case-fold-search nil))
                      (looking-at org-complex-heading-regexp))
                    (let ((begin (point))
                          (heading (match-string-no-properties 4)))
                      (unless (or (and
                                   org-refile-target-verify-function
                                   (not
                                    (funcall org-refile-target-verify-function)))
                                  (not heading))
                        (let ((re (format org-complex-heading-regexp-format
                                          (regexp-quote heading)))
                              (target
                               (if (not org-refile-use-outline-path) heading
                                 (concat
                                  (file-name-nondirectory (buffer-file-name (buffer-base-buffer)))
                                  " ✦ "
                                  (org-format-outline-path (org-get-outline-path t t) 1000 nil " ➜ ")
                                  ))))

                          (push (list target f re (org-refile-marker (point)))
                                tgs)))
                      (when (= (point) begin)
                        ;; Verification function has not moved point.
                        (end-of-line)))))))
              (when org-refile-use-cache
                (org-refile-cache-put tgs (buffer-file-name) descre))
              (setq targets (append tgs targets))))))
      (message "Getting targets...done")
      (delete-dups (nreverse targets))))

  (defun spacemacs//org-ctrl-c-ctrl-c-counsel-org-tag ()
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
  (add-hook 'org-ctrl-c-ctrl-c-hook 'spacemacs//org-ctrl-c-ctrl-c-counsel-org-tag)
  )
