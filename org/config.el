;;; private/org/init.el -*- lexical-binding: t; -*-

(defvar +org-dir (expand-file-name "~/Dropbox/org/")
  "The directory where org files are kept.")

;; Ensure ELPA org is prioritized above built-in org.
(when-let* ((path (locate-library "org" nil doom--package-load-path)))
  (setq load-path (delete path load-path))
  (push (file-name-directory path) load-path))

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

(use-package evil-org
             :ensure t
             :after org
             ;; :config
             ;; (add-hook 'org-mode-hook 'evil-org-mode)
             ;; (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))
             )

;;;;; Org-super-agenda
(def-package! org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups
        '((:name "Log "
                 :log t)  ; Automatically named "Log"
          (:name "Schedule "
                 :time-grid t)
          (:name "Today "
                 :scheduled today)
          (:name "Habits "
                 :habit t)
          (:name "Due today "
                 :deadline today)
          (:name "Overdue "
                 :deadline past)
          (:name "Due soon "
                 :deadline future)
          (:name "Waiting "
                 :todo "WAIT"
                 :order 98)
          (:name "Scheduled earlier "
                 :scheduled past)))
  (org-super-agenda-mode))

;;
;; Bootstrap
;;

(after! org
  ;; Occasionally, Emacs encounters an error loading the built-in org, aborting
  ;; the load. This results in a broken, partially loaded state. This require
  ;; tries to set it straight.
  (require 'org)

  (defvaralias 'org-directory '+org-dir)

  (+org-init-ui)
  (+org-hacks)
  (+org-overrides)

  (push 'org-agenda-mode evil-snipe-disabled-modes)
  (add-hook 'org-agenda-mode-hook #'(lambda () (evil-vimish-fold-mode -1)))
  (set! :evil-state 'org-agenda-mode 'motion))

(add-hook! org-mode
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


(map!
 (:mode org-mode
   :map org-mode-map
   "RET" #'org-return-indent
   "M-o" #'org-open-at-point
   "M-i" #'org-insert-last-stored-link
   "M-I" #'org-insert-link
   "s-p" #'org-ref-ivy-insert-cite-link

   ;; "C-c C-S-l" #'+org/remove-link
   ;;                          :n "C-c C-i" #'org-toggle-inline-images
   (:localleader
     :desc "Schedule"          :n "s"  #'org-schedule
     :desc "Remove link"       :n "s"  #'+org/remove-link
     :desc "Deadline"          :n "d"  #'org-deadline
     :desc "C-c C-c"           :n "m"  #'org-ctrl-c-ctrl-c
     :desc "Edit Special"      :n "'"  #'org-edit-special
     :desc "Effort"            :n "e"  #'org-set-effort
     :desc "TODO"              :n "t"  #'org-todo
     :desc "Clocking Effort"   :n "E"  #'org-clock-modify-effort-estimate
     :desc "Property"          :n "p"  #'org-set-property
     :desc "Clock-in"          :n "i"  #'org-clock-clock-in
     :desc "Clock-out"         :n "o"  #'org-clock-clock-out
     :desc "Narrow to Subtree" :n "n"  #'org-narrow-to-subtree
     :desc "Narrow to Element" :n "N"  #'org-narrow-to-element
     :desc "Widen"             :n "w"  #'widen
     :desc "Toggle heading"    :n "h"  #'org-toggle-heading
     :desc "Toggle Image"      :n "ci" #'org-toggle-inline-images
     :desc "Toggle Latex"      :n "cl" #'org-toggle-latex-fragment
     :desc "Archive Subtree"   :n "A"  #'org-archive-subtree
     :desc "Toggle Archive"    :n "a"  #'org-toggle-archive-tag
     )
   :n  "RET" #'+org/dwim-at-point
   :n  [tab]     #'org-cycle
   :i  [tab]     #'+org/indent-or-next-field-or-yas-expand
   :i  "<S-tab>" #'+org/dedent-or-prev-field

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
     :m "<escape>" #'org-agenda-Quit
     :m "\\" #'ace-window
     :m "t" #'org-agenda-todo
     :m "p" #'org-set-property
     :m "e" #'org-agenda-set-effort
     :m "h" #'org-habit-toggle-habits
     :m "l" #'org-agenda-log-mode
     :m "D" #'org-agenda-toggle-diary
     :m "G" #'org-agenda-toggle-time-grid
     :m ";" #'counsel-org-tag-agenda
     :m "s-j" #'counsel-org-goto-all

     :m "i" #'org-agenda-clock-in
     :m "o" #'org-agenda-clock-out

     :m "C" #'org-agenda-capture
     :m "m" #'org-agenda-bulk-mark
     :m "u" #'org-agenda-bulk-unmark
     :m "U" #'org-agenda-bulk-unmark-all

     :m "f;" #'org-agenda-filter-by-tag
     :m "fh" #'org-agenda-filter-by-top-headline
     :m "fc" #'org-agenda-filter-by-category
     :m "fe" #'org-agenda-filter-by-effort
     :m "fr" #'org-agenda-filter-by-regexp
     :m "ff" #'org-agenda-filter-remove-all

     :m "-" #'org-agenda-manipulate-query-subtract
     :m "=" #'org-agenda-manipulate-query-add
     :m "_" #'org-agenda-manipulate-query-subtract-re
     :m "$" #'org-agenda-manipulate-query-add-re

     :m "d" #'org-agenda-deadline
     :m "s" #'org-agenda-schedule

     :m "S" #'org-save-all-org-buffers
     ))
 )
