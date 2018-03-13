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

(after! org-agenda
  (org-super-agenda-mode)
  (def-hydra! +org@org-agenda-filter (:color pink :hint nil)
    "
_;_ tag      _h_ headline      _c_ category     _r_ regexp     _d_ remove    "
    (";" org-agenda-filter-by-tag)
    ("h" org-agenda-filter-by-top-headline)
    ("c" org-agenda-filter-by-category)
    ("r" org-agenda-filter-by-regexp)
    ("d" org-agenda-filter-remove-all)
    ("q" nil "cancel" :color blue))
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
  (set! :popup "^\\*Org Agenda.*\\*$" '((slot . -1) (size . 120) (side . right)) '((select . t) (modeline . nil)))
  (push 'org-agenda-mode evil-snipe-disabled-modes)
  (add-hook 'org-agenda-finalize-hook #'hide-mode-line-mode)
  (set! :evil-state 'org-agenda-mode 'normal))
