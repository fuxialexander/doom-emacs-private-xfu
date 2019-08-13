;;; lang/org-private/autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org-move-point-to-heading ()
        (cond ((org-at-heading-p) (org-beginning-of-line))
              (t (org-previous-visible-heading 1))))
;;;###autoload
(defun export-diary-from-cal ()
  (interactive)
  (start-process-shell-command "export diary" nil "~/.emacs.d-backup/private/local/calendardiary 30 > ~/Dropbox/org/cal.diary"))

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
(defun +org-toggle-math ()
  (interactive)
  (if (bound-and-true-p org-cdlatex-mode)
      (org-cdlatex-mode -1)
    (org-cdlatex-mode +1))
  (if (bound-and-true-p webkit-katex-render-mode)
      (webkit-katex-render-mode -1)
    (webkit-katex-render-mode +1)))
