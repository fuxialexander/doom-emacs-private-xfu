;;; private/org/autoload/org-todo.el -*- lexical-binding: t; -*-

;;;###autoload
(defun =calendar ()
  "Activate (or switch to) `calendar' in its workspace."
  (interactive)
  (unless (featurep! :feature workspaces)
    (user-error ":feature workspaces is required, but disabled"))
  (+workspace-switch "Calendar" t)
  (if-let* ((buf (cl-find-if (lambda (it) (string-match-p "^\\*cfw" (buffer-name (window-buffer it))))
                             (doom-visible-windows))))
      (select-window (get-buffer-window buf)) (call-interactively 'my-open-calendar))
  (+workspace/display))

