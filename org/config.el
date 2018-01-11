;;; private/org/config.el -*- lexical-binding: t; -*-

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

(def-package! org-bullets
  :commands org-bullets-mode
  :config
  (setq org-bullets-bullet-list '("#" "#" "#" "#" "#" "#" "#" "#")))

(def-package! org-brain
  :after org
  :init
  (setq org-brain-path "~/Dropbox/org/brain")
  (push 'org-agenda-mode evil-snipe-disabled-modes)
  (add-hook 'org-agenda-mode-hook #'(lambda () (evil-vimish-fold-mode -1)))
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
        org-brain-title-max-length 12)
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
     :n "o" 'link-hint-open-link
     :n "q" 'org-brain-visualize-quit
     :n "r" 'org-brain-visualize-random
     :n "R" 'org-brain-visualize-wander
     :n "s" 'org-brain-visualize
     :n "S" 'org-brain-goto
     :n [tab] 'org-brain-goto-current
     )))

;;;;; Org-super-agenda
(def-package! org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups
        '((:name "Log\n"
                 :log t)  ; Automatically named "Log"
          (:name "Schedule\n"
                 :time-grid t)
          (:name "Today\n"
                 :scheduled today)
          (:name "Habits\n"
                 :habit t)
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
                 :scheduled past)))
  (org-super-agenda-mode))

;;
;; Bootstrap
;;
(add-hook! 'org-load-hook

  (+org-init-ui)
  (+org-hacks)
  (+org-overrides)


  (push 'org-agenda-mode evil-snipe-disabled-modes)
  (add-hook 'org-agenda-mode-hook #'(lambda () (evil-vimish-fold-mode -1)))
  (set! :evil-state 'org-agenda-mode 'motion))

(add-hook! 'org-mode-hook
  #'(doom|disable-line-numbers  ; no line numbers
     org-bullets-mode           ; "prettier" bullets
     org-indent-mode            ; margin-based indentation
     toc-org-enable             ; auto-table of contents
     ;; visual-line-mode           ; line wrapping

     +org|enable-auto-reformat-tables
     +org|enable-auto-update-cookies
     +org|smartparens-compatibility-config
     +org|unfold-to-2nd-level-or-point
     +org|show-paren-mode-compatibility
     ))

(def-hydra! +org@org-agenda-filter (:color pink :hint nil)
  "
_;_ tag      _h_ headline      _c_ category     _r_ regexp     _d_ remove    "
  (";" org-agenda-filter-by-tag)
  ("h" org-agenda-filter-by-top-headline)
  ("c" org-agenda-filter-by-category)
  ("r" org-agenda-filter-by-regexp)
  ("d" org-agenda-filter-remove-all)
  ("q" nil "cancel" :color blue))

(map!
 (:after org
   :map org-mode-map
   "RET" #'org-return-indent
   "M-o" #'org-open-at-point
   "M-i" #'org-insert-last-stored-link
   "M-I" #'org-insert-link
   "s-p" #'org-ref-ivy-insert-cite-link
   :n  "RET" #'+org/dwim-at-point
   :n  [tab]     #'org-cycle
   :n  "t"       #'org-todo
   :n  "T"       #'org-insert-todo-heading-respect-content
   :i  [tab]     #'+org/indent-or-next-field-or-yas-expand
   :i  "<S-tab>" #'+org/dedent-or-prev-field

   ;; "C-c C-S-l" #'+org/remove-link
   ;;                          :n "C-c C-i" #'org-toggle-inline-images
   (:localleader
     :desc "Schedule"          :n "s"                  #'org-schedule
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
     :desc "Toggle Image"      :n "ci"                 #'org-toggle-inline-images
     :desc "Toggle Latex"      :n "cl"                 #'org-toggle-latex-fragment
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
     :m "C-k"      #'evil-window-up
     :m "C-j"      #'evil-window-down
     :m "C-h"      #'evil-window-left
     :m "C-l"      #'evil-window-right
     :m "<escape>" #'org-agenda-Quit
     :m [tab]      #'+my-org-agenda-tree-to-indirect-buffer
     :m "\\"       #'ace-window
     :m "t"        #'org-agenda-todo
     :m "p"        #'org-set-property
     :m "e"        #'org-agenda-set-effort
     :m "h"        #'org-habit-toggle-habits
     :m "l"        #'org-agenda-log-mode
     :m "D"        #'org-agenda-toggle-diary
     :m "G"        #'org-agenda-toggle-time-grid
     :m ";"        #'counsel-org-tag-agenda
     :m "s-j"      #'counsel-org-goto-all

     :m "i"        #'org-agenda-clock-in
     :m "o"        #'org-agenda-clock-out

     :m "J"        #'org-agenda-later
     :m "K"        #'org-agenda-earlier

     :m "C"        #'org-agenda-capture
     :m "m"        #'org-agenda-bulk-mark
     :m "u"        #'org-agenda-bulk-unmark
     :m "U"        #'org-agenda-bulk-unmark-all

     :m "f"        #'+org@org-agenda-filter/body

     :m "-"        #'org-agenda-manipulate-query-subtract
     :m "="        #'org-agenda-manipulate-query-add
     :m "_"        #'org-agenda-manipulate-query-subtract-re
     :m "$"        #'org-agenda-manipulate-query-add-re

     :m "d"        #'org-agenda-deadline
     :m "s"        #'org-agenda-schedule

     :m "z"        #'org-agenda-view-mode-dispatch
     :m "S"        #'org-save-all-org-buffers
     ))
 (:after org-src
   (:map org-src-mode-map
     (:localleader
       :desc "Finish" :n "'"  #'org-edit-src-exit
       :desc "Finish" :n doom-localleader-key  #'org-edit-src-exit
       :desc "Abort"  :n "k"  #'org-edit-src-abort
       )))
 (:after org-capture
   (:map org-capture-mode-map
     (:localleader
       :desc "Finish" :n "'"  #'org-edit-src-exit
       :desc "Finish" :n doom-localleader-key  #'org-edit-src-exit
       :desc "Abort"  :n "k"  #'org-edit-src-abort
       )))
 )

(after! org
  (defvaralias 'org-directory '+org-dir)
  (when (featurep 'org)
    (run-hooks 'org-load-hook))
  (set! :popup "^CAPTURE.*\\.org$" '((side . bottom) (size . 0.4)) '((select . t)))
  (set! :popup "^\\*Org Agenda\\*$" '((slot . -1) (size . 120) (side . right)) '((select . t)))
  (set! :popup "^\\*Org Src" '((size . 0.4) (side . right)) '((quit) (select . t)))

  (defun +my-org-tree-to-indirect-buffer (&optional args)
    "create indirect buffer and narrow it to current subtree."
    (interactive "P")
    (let ((cbuf (current-buffer))
          (cwin (selected-window))
          (pos (point))
          beg end level heading ibuf)
      (save-excursion
        (org-back-to-heading t)
        (setq beg (point)
              heading (org-get-heading 'no-tags))
        (org-end-of-subtree t t)
        (when (org-at-heading-p) (backward-char 1))
        (setq end (point)))
      (setq ibuf (org-get-indirect-buffer cbuf heading)
            org-last-indirect-buffer ibuf)
      (+popup-buffer ibuf '((side . right) (slot . 0) (transient . t) (window-height . 0.6)))
      (select-window (get-buffer-window ibuf))
      (narrow-to-region beg end)
      (visual-line-mode)
      (outline-show-all)
      (goto-char pos)
      (run-hook-with-args 'org-cycle-hook 'all)
      ))

  (defun +my-org-agenda-tree-to-indirect-buffer (arg)
    "Same as `org-agenda-tree-to-indirect-buffer' without saving window."
    (interactive "P")
    (org-agenda-check-no-diary)
    (let* ((agenda-window (selected-window))
           (marker (or (org-get-at-bol 'org-marker)
                       (org-agenda-error)))
           (buffer (marker-buffer marker))
           (pos (marker-position marker)))
      (with-current-buffer buffer
        (save-excursion
          (goto-char pos)
          (funcall '+my-org-tree-to-indirect-buffer arg)))
        (select-window agenda-window)))
  )
