;;; private/org/+todo.el -*- lexical-binding: t; -*-
(add-hook 'org-load-hook #'+org|init-todo)

;;
;; Plugins
;;

(def-package! org-super-agenda
  :commands (org-super-agenda-mode)
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
                 :scheduled past))))

(def-package! org-clock-budget :load-path "~/Source/playground/org-clock-budget"
  :commands (org-clock-budget-report)
  :init
  (defun my-buffer-face-mode-org-clock-budget ()
    "Sets a fixed width (monospace) font in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "input mono compressed" :height 1.0))
    (buffer-face-mode)
    (setq-local line-spacing nil))
  :config
  (add-hook! 'org-clock-budget-report-mode-hook
    (toggle-truncate-lines 1)
    (my-buffer-face-mode-org-clock-budget)))
(def-package! org-clock-convenience
  :commands (org-clock-convenience-timestamp-up
             org-clock-convenience-timestamp-down
             org-clock-convenience-fill-gap
             org-clock-convenience-fill-gap-both))

(def-package! org-wild-notifier
  :commands (org-wild-notifier-mode
             org-wild-notifier-check)
  :config
  (setq org-wild-notifier-keyword-whitelist '("TODO" "HABT")))

(defun +org|init-todo ()
  (defface org-todo-keyword-todo '((t ())) "org-todo" :group 'org)
  (defface org-todo-keyword-kill '((t ())) "org-kill" :group 'org)
  (defface org-todo-keyword-outd '((t ())) "org-outd" :group 'org)
  (defface org-todo-keyword-wait '((t ())) "org-wait" :group 'org)
  (defface org-todo-keyword-done '((t ())) "org-done" :group 'org)
  (defface org-todo-keyword-habt '((t ())) "org-habt" :group 'org)
  (push 'org-agenda-mode evil-snipe-disabled-modes)
  (add-hook 'org-agenda-finalize-hook #'doom-hide-modeline-mode)
  ;; (add-hook! 'org-agenda-finalize-hook (org-wild-notifier-mode 1))
  (set! :evil-state 'org-agenda-mode 'normal)

  (setq org-agenda-block-separator ""
        org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 3 :fileskip0 t :stepskip0 t :tags "-COMMENT"))
        org-agenda-compact-blocks t
        org-agenda-dim-blocked-tasks nil
        org-agenda-files (append (list "/Users/xfu/Dropbox/org/cal/cal.org") (ignore-errors (directory-files +org-dir t "\\.org$" t)))
        org-agenda-follow-indirect t
        org-agenda-ignore-properties '(effort appt category)
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
        org-agenda-use-tag-inheritance nil

        org-complete-tags-always-offer-all-agenda-tags t

        org-clock-clocktable-default-properties (quote (:maxlevel 3 :scope agenda :tags "-COMMENT"))
        org-clock-persist t
        org-clock-persist-file (expand-file-name ".org-clock-persist-data.el" +org-dir)
        org-clocktable-defaults (quote (:maxlevel 3 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 t :fileskip0 t :tags "-COMMENT" :emphasize nil :link nil :narrow 40! :indent t :formula nil :timestamp nil :level nil :tcolumns nil :formatter nil))
        org-columns-default-format "%50ITEM(Task) %8CLOCKSUM %16TIMESTAMP_IA"
        org-enforce-todo-dependencies t
        org-global-properties (quote (("Effort_ALL" . "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00")))
        org-habit-following-days 0
        org-habit-graph-column 1
        org-habit-preceding-days 8
        org-habit-show-habits t
        org-hide-block-startup t
        org-log-done 'time
        org-log-into-drawer t
        org-log-note-clock-out t
        org-log-redeadline 'time
        org-log-reschedule 'time
        org-log-state-notes-into-drawer t
        org-outline-path-complete-in-steps nil
        org-publish-timestamp-directory (concat +org-dir ".org-timestamps/")
        org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9))
        org-refile-use-outline-path 'file
        org-tags-column 0
        org-lowest-priority ?F
        org-highest-priority ?A
        org-todo-keyword-faces '(("TODO" . org-todo-keyword-todo)
                                 ("HABT" . org-todo-keyword-habt)
                                 ("DONE" . org-todo-keyword-done)
                                 ("WAIT" . org-todo-keyword-wait)
                                 ("KILL" . org-todo-keyword-kill)
                                 ("OUTD" . org-todo-keyword-outd))
        org-todo-keywords '(
                            (sequence "TODO(t!)"  "|" "DONE(d!/@)")
                            (sequence "WAIT(w@/@)" "|" "OUTD(o@/@)" "KILL(k@/@)")
                            (sequence "HABT(h!)" "|" "DONE(d!/@)" "KILL(k@/@)"))
        org-treat-insert-todo-heading-as-state-change t
        org-use-fast-tag-selection nil
        org-use-fast-todo-selection t)
  (def-hydra! +org@org-agenda-filter (:color pink :hint nil)
    "
_;_ tag      _h_ headline      _c_ category     _r_ regexp     _d_ remove    "
    (";" org-agenda-filter-by-tag)
    ("h" org-agenda-filter-by-top-headline)
    ("c" org-agenda-filter-by-category)
    ("r" org-agenda-filter-by-regexp)
    ("d" org-agenda-filter-remove-all)
    ("q" nil "cancel" :color blue))

  )


(after! org-agenda
    (org-super-agenda-mode)
    (defun org-agenda-align-tags (&optional line)
    "Align all tags in agenda items to `org-agenda-tags-column'."
    (let ((inhibit-read-only t)
          (org-agenda-tags-column (if (eq 'auto org-agenda-tags-column)
                                      (- (- (window-text-width) 2))
                                    org-agenda-tags-column))
          l c)
      (save-excursion
        (goto-char (if line (point-at-bol) (point-min)))
        (while (re-search-forward "\\([ \t]+\\)\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$"
                                  (if line (point-at-eol) nil) t)
          (add-text-properties
           (match-beginning 2) (match-end 2)
           (list 'face (delq nil (let ((prop (get-text-property
                                              (match-beginning 2) 'face)))
                                   (or (listp prop) (setq prop (list prop)))
                                   (if (memq 'org-tag prop)
                                       prop
                                     (cons 'org-tag prop))))))
          (setq l (- (match-end 2) (match-beginning 2))
                c (if (< org-agenda-tags-column 0)
                      (- (abs org-agenda-tags-column) l)
                    org-agenda-tags-column))
          (delete-region (match-beginning 1) (match-end 1))
          (goto-char (match-beginning 1))
          (insert (org-add-props
		              (make-string (max 1 (- c (current-column))) ?\ )
		              (plist-put (copy-sequence (text-properties-at (point)))
			                     'face nil))))
        (goto-char (point-min))
        (org-font-lock-add-tag-faces (point-max)))))
    (defun start-org-wild-notifier ()
      (if (bound-and-true-p org-wild-notifier-mode)
          (message "You already have notifier with you!")
      (run-with-timer 60 nil 'org-wild-notifier-mode 1)
      (message "Org wild notifier, naughty naughty fire!")))
    (start-org-wild-notifier)
    )
