;;; private/org/+todo.el -*- lexical-binding: t; -*-
(add-hook 'org-load-hook #'+org|init-todo)
(defun +org|init-todo ()
  (defface org-todo-keyword-todo '((t ())) "org-todo" :group 'org)
  (defface org-todo-keyword-kill '((t ())) "org-kill" :group 'org)
  (defface org-todo-keyword-outd '((t ())) "org-outd" :group 'org)
  (defface org-todo-keyword-wait '((t ())) "org-wait" :group 'org)
  (defface org-todo-keyword-done '((t ())) "org-done" :group 'org)
  (defface org-todo-keyword-habt '((t ())) "org-habt" :group 'org)

  (setq-default

   org-clock-persist-file (concat doom-cache-dir "org-clock-save.el")
   org-id-locations-file (concat doom-cache-dir ".org-id-locations")
   org-publish-timestamp-directory (concat doom-cache-dir ".org-timestamps/")
   org-agenda-follow-indirect t
   diary-file "/Users/xfu/Dropbox/org/cal.diary"
   ;; org-default-notes-file "/Users/xfu/Dropbox/org/inbox.org"
   org-log-done 'time
   org-log-note-clock-out t
   org-log-redeadline 'note
   org-log-reschedule 'note
   org-enforce-todo-dependencies t
   org-habit-graph-column 1
   org-habit-following-days 0
   org-habit-preceding-days 8
   org-habit-show-habits t
   Org-hide-block-startup t
   org-tags-column 0
   org-tag-persistent-alist '(
                              (:startgrouptag)
                              ("Mood")
                              (:grouptags)
                              ("happy")
                              ("sad")
                              ("pity")
                              ("excited")
                              ("bored")
                              ("tired")
                              ("giveup")
                              ("eager")
                              ("hope")
                              ("warm")
                              (:endgrouptag)

                              (:startgrouptag)
                              ("People")
                              (:grouptags)
                              ("KevinGroup")
                              ("hscr")
                              ("Friends")
                              ("Family")
                              ("Office")
                              ("Acquaintances")
                              (:endgrouptag)

                              (:startgrouptag)
                              ("hscr")
                              (:grouptags)
                              ("merce")
                              ("pak")
                              ("clara")
                              ("elly")
                              ("kathy")
                              (:endgrouptag)

                              (:startgrouptag)
                              ("KevinGroup")
                              (:grouptags)
                              ("kevin")
                              ("qin")
                              ("danny")
                              ("qiong")
                              ("christina")
                              ("yong")
                              ("chengyang")
                              ("kelly")
                              ("le")
                              ("saudan")
                              (:endgrouptag)

                              (:endgrouptag)

                              (:startgrouptag)
                              ("statistics")
                              (:grouptags)
                              ("hypo_test")
                              ("bayesian")
                              ("linear_model")
                              ("penalized_regression")
                              ("significance")
                              ("fdr")
                              (:endgrouptag)

                              (:startgrouptag)
                              ("math")
                              (:grouptags)
                              ("linear_algebra")
                              ("calculus")
                              ("fdr")
                              ("optimization")
                              (:endgrouptag)

                              (:startgrouptag)
                              ("Research")
                              (:grouptags)
                              ("hic")
                              ("hscr")
                              ("search")
                              ("read")
                              ("think")
                              ("literature")
                              ("website")
                              ("learn")
                              ("coding")
                              ("analysis")
                              ("organize")
                              ("plot")
                              ("discuss")
                              ("talk")
                              (:endgrouptag)


                              )
   org-agenda-tags-column 'auto
   org-agenda-restore-windows-after-quit t
   org-agenda-span 'day
   org-agenda-files '("/Users/xfu/Dropbox/org/")
   org-agenda-restore-windows-after-quit t
   org-agenda-sticky nil
   org-agenda-block-separator ""
   org-agenda-overriding-header ""
   org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 3 :fileskip0 t :stepskip0 t :tags "-COMMENT"))
   org-agenda-dim-blocked-tasks (quote invisible)
   org-agenda-log-mode-items (quote (closed clock))
   org-agenda-skip-deadline-if-done t
   org-agenda-skip-deadline-prewarning-if-scheduled t
   org-agenda-skip-scheduled-if-done t
   org-agenda-start-with-log-mode nil
   org-agenda-custom-commands '()
   org-agenda-sorting-strategy
   '((agenda time-up priority-down category-keep)
     (todo   priority-down category-keep)
     (tags   priority-down category-keep)
     (search category-keep))
   org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 9))
   org-refile-use-outline-path 'file
   org-outline-path-complete-in-steps nil
   org-columns-default-format "%50ITEM(Task) %8CLOCKSUM %16TIMESTAMP_IA"
   org-global-properties (quote (("Effort_ALL" . "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00")))
   org-use-fast-todo-selection t
   org-use-fast-tag-selection nil
   org-treat-insert-todo-heading-as-state-change t
   org-log-into-drawer t
   org-log-state-notes-into-drawer t
   org-clock-clocktable-default-properties (quote (:maxlevel 3 :scope agenda :tags "-COMMENT"))
   org-clocktable-defaults
   (quote (:maxlevel 3 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 t :fileskip0 t :tags "-COMMENT" :emphasize nil :link nil :narrow 40! :indent t :formula nil :timestamp nil :level nil :tcolumns nil :formatter nil))
   org-todo-keywords '(
                       (sequence "TODO(t!)"  "|" "DONE(d!/@)")
                       (sequence "WAIT(w@/@)" "|" "OUTD(o@/@)" "KILL(k@/@)")
                       (sequence "HABT(h!)" "|" "DONE(d!/@)" "KILL(k@/@)"))
   org-todo-keyword-faces '(("TODO" . org-todo-keyword-todo)
                            ("HABT" . org-todo-keyword-habt)
                            ("DONE" . org-todo-keyword-done)
                            ("WAIT" . org-todo-keyword-wait)
                            ("KILL" . org-todo-keyword-kill)
                            ("OUTD" . org-todo-keyword-outd)))
  (defun my-org-after-todo-state-change-action ()
    (pcase org-state
      ("WAIT" (org-schedule '(4)))
      ("TODO" (org-schedule '(0)))
      ('nil (org-schedule '(4)) (org-deadline '(4)))))
  (add-hook 'org-after-todo-state-change-hook #'my-org-after-todo-state-change-action)
  (defun myorg-agenda-format-date-aligned (date)
    "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading."
    (require 'cal-iso)
    (let* ((dayname (calendar-day-name date))
           (day (cadr date))
           (day-of-week (calendar-day-of-week date))
           (month (car date))
           (monthname (calendar-month-name month))
           (year (nth 2 date))
           (iso-week (org-days-to-iso-week
                      (calendar-absolute-from-gregorian date)))
           (weekyear (cond ((and (= month 1) (>= iso-week 52))
                            (1- year))
                           ((and (= month 12) (<= iso-week 1))
                            (1+ year))
                           (t year)))
           (weekstring (if (= day-of-week 1)
                           (format " W%02d" iso-week)
                         "")))
      (format "%-10s %2d %s %4d%s\n"
              dayname day monthname year weekstring)))
  (setq org-agenda-format-date 'myorg-agenda-format-date-aligned)
  ;; (add-hook 'org-agenda-finalize-hook 'place-agenda-tags)
  )

(def-package! calfw
  :commands (cfw:open-calendar-buffer)
  :config

  ;; Unicode characters
  (setq
   cfw:face-item-separator-color nil
   cfw:fchar-junction ?╋
   cfw:fchar-vertical-line ?┃
   cfw:fchar-horizontal-line ?━
   cfw:fchar-left-junction ?┣
   cfw:fchar-right-junction ?┫
   cfw:fchar-top-junction ?┯
   cfw:fchar-top-left-corner ?┏
   cfw:fchar-top-right-corner ?┓)


  (defun cfw:render-button (title command &optional state)
    "render-button
 TITLE
 COMMAND
 STATE"
    (let ((text (concat " " title " "))
          (keymap (make-sparse-keymap)))
      (cfw:rt text (if state 'cfw:face-toolbar-button-on
                     'cfw:face-toolbar-button-off))
      (define-key keymap [mouse-1] command)
      (cfw:tp text 'keymap keymap)
      (cfw:tp text 'mouse-face 'highlight)
      text))


  ;; (push '(;; …
  ;;         ("k" "calfw, non-kevin"
  ;;    cfw:open-org-calendar-nonwork))
  ;;       org-agenda-custom-commands
  ;;       )
  (add-hook! 'cfw:calendar-mode-hook (solaire-mode +1)
    (doom-hide-modeline-mode))
  )

(def-package! calfw-org
    :commands (cfw:open-org-calendar
               cfw:org-create-source
               cfw:open-org-calendar-withkevin
               my-open-calendar)
    :config
    (defun my-open-calendar ()
      (interactive)
      (let ((cfw:calendar-mode-map nil))
        (cfw:open-calendar-buffer
         ;; :custom-map cfw:my-cal-map
         :contents-sources
         (list
          (cfw:org-create-source (doom-color 'fg))  ; orgmode source
          ))))
    (defun cfw:open-org-calendar-withkevin ()
      (interactive)
      (let ((org-agenda-files '("~/Dropbox/org/" "~/Dropbox/org/cal/")))
        (my-open-calendar))))

(def-package! org-gcal
    :commands (org-gcal-sync
               org-gcal-fetch
               org-gcal-post-at-point
               org-gcal-delete-at-point)
    :config

    (defun org-gcal--notify (title mes)
      (message "org-gcal::%s - %s" title mes)))

(def-package! alert)
