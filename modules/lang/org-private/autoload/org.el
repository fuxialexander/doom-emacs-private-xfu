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

