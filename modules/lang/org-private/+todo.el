;;; lang/org-private/+todo.el -*- lexical-binding: t; -*-

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
    (setq buffer-face-mode-face '(:family "PragmataPro" :height 1.0))
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

(def-package! org-clock-convenience
  :commands (org-clock-convenience-timestamp-up
             org-clock-convenience-timestamp-down
             org-clock-convenience-fill-gap
             org-clock-convenience-fill-gap-both))


(after! org-agenda
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

  (set! :popup "^\\*Org Agenda.*" '((slot . -1) (size . 120) (side . left)) '((select . t) (modeline . nil)))
  (after! evil-snipe
    (push 'org-agenda-mode evil-snipe-disabled-modes))
  (set! :evil-state 'org-agenda-mode 'normal))
