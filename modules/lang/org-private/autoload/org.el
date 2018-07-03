;;; lang/org-private/autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload
(defun export-diary-from-cal ()
  (interactive)
  (start-process-shell-command "export diary" nil "/Users/xfu/.emacs.d-backup/private/local/calendardiary 30 > /Users/xfu/Dropbox/org/cal.diary"))

;;;###autoload
(defun +org/open-brain-here ()
    (interactive)
    (let ((org-brain-path (projectile-project-root)))
      (call-interactively 'org-brain-visualize)))

;;;###autoload
(defun reflash-indentation ()
  "Fix org-indent issues, center line."
  (interactive)
  (org-indent-mode 1)
  (recenter-top-bottom))


;;;###autoload
(defun +org/work-on-heading ()
  (interactive)
  (org-clock-in)
  (+workspace-switch (nth 4 (org-heading-components)) t)
  (delete-other-windows)
  (org-tree-to-indirect-buffer)
  (other-window 1)
  (delete-other-windows)
  (+workspace/display)
  (start-process-shell-command "Focus" nil "open -g \"focus://focus\"")
  (map! :map evil-org-mode-map
        :ni "<s-return>" #'+org/finish-work-on-heading
        :ni "<S-s-return>"        #'+org/cancel-work-on-heading))

;;;###autoload
(defun +org/finish-work-on-heading ()
  (interactive)
  (setq *org-git-notes (nth 4 (org-heading-components)))
  (org-clock-out)
  (save-buffer)
  (kill-this-buffer)
  (+workspace/delete (+workspace-current-name))
  (start-process-shell-command "Unfocus" nil "open -g \"focus://unfocus\"")
  (map! :map evil-org-mode-map
        :ni "<s-return>" #'+org/work-on-heading
        :ni "<S-s-return>" nil))

;;;###autoload
(defun +org/cancel-work-on-heading ()
  (interactive)
  (setq *org-git-notes nil)
  (org-clock-cancel)
  (save-buffer)
  (kill-this-buffer)
  (+workspace/delete (+workspace-current-name))
  (start-process-shell-command "Unfocus" nil "open -g \"focus://unfocus\"")
  (map! :map evil-org-mode-map
        :ni "<s-return>" #'+org/work-on-heading
        :ni "<S-s-return>" nil))

;;;###autoload
(defun *org/shiftcontroldown (&optional n)
  "Change timestamps synchronously down in CLOCK log lines.
Optional argument N tells to change by that many units."
  (interactive "P")
  (if (and (org-at-clock-log-p) (org-at-timestamp-p 'lax))
      (let (org-support-shift-select)
        (org-clock-timestamps-down n))
    (org-timestamp-down)))

;;;###autoload
(defun *org/shiftcontrolup (&optional n)
  "Change timestamps synchronously up in CLOCK log lines.
Optional argument N tells to change by that many units."
  (interactive "P")
  (if (and (org-at-clock-log-p) (org-at-timestamp-p 'lax))
      (let (org-support-shift-select)
        (org-clock-timestamps-up n))
    (org-timestamp-up)))

;;;###autoload
(defun +org-private|toggle-only-current-fold ()
  "Toggle the local fold at the point (as opposed to cycling through all levels
with `org-cycle')."
  (interactive)
  (save-excursion
    (org-beginning-of-line)
    (cond ((org-at-heading-p)
           (outline-toggle-children)
           (unless (outline-invisible-p (line-end-position))
             (org-cycle-hide-drawers 'subtree))
           t))))

;;;###autoload
(defun +org-private*evil-org-open-below (count)
  "Clever insertion of org item.
Argument COUNT number of lines to insert.
The behavior in items and tables can be controlled using ‘evil-org-special-o/O’.
Passing in any prefix argument, executes the command without special behavior."
  (interactive "P")
  (cond ((and (memq 'table-row evil-org-special-o/O) (org-at-table-p))
         (org-table-insert-row '(4))
         (evil-insert nil))
        ((and (memq 'item evil-org-special-o/O) (org-at-item-p)
              (progn (org-end-of-item)
                     (backward-char 1)
                     (evil-append nil)
                     (org-insert-item (org-at-item-checkbox-p))))
         (evil-insert nil))
        ((evil-open-below count))))

;;;###autoload
(defun +org-private*org-meta-return (&optional arg)
  "Insert a new heading or wrap a region in a table.
Calls `org-insert-heading', `org-insert-item' or
`org-table-wrap-region', depending on context.  When called with
an argument, unconditionally call `org-insert-heading'."
  (interactive "P")
  (org-check-before-invisible-edit 'insert)
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (call-interactively (cond (arg #'org-insert-heading)
				((org-at-table-p) #'org-table-wrap-region)
				((org-in-item-p) (lambda! (org-end-of-item)
                                  (backward-char 1)
                                  (evil-append nil)
                                  (org-insert-item (org-at-item-checkbox-p))))
				(t #'org-insert-heading)))))
